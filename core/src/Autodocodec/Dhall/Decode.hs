{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Autodocodec.Dhall.Decode
  ( null,
    bool,
    string,
    integer,
    number,
    hashMap,
    map,
    value,
    arrayOf,
    objectOf,
    eq,
    bimap,
    bimap',
    either,
    discriminatedUnion,
    comment,
    reference,
    requiredKey,
    optionalKey,
    optionalKeyWithDefault,
    optionalKeyWithOmittedDefault,
    pure,
    ap,
  )
where

import "aeson" Data.Aeson
  ( FromJSONKey,
    FromJSONKeyFunction
      ( FromJSONKeyCoerce,
        FromJSONKeyText,
        FromJSONKeyTextParser,
        FromJSONKeyValue
      ),
    fromJSONKey,
  )
import "aeson" Data.Aeson qualified as JSON
import "autodocodec" Autodocodec qualified as Autodo
import safe "base" Control.Applicative ((<*>))
import safe "base" Control.Applicative qualified as Base (pure)
import safe "base" Control.Arrow ((&&&))
import safe "base" Control.Category (id, (.))
import safe "base" Control.Monad ((<=<))
import safe "base" Data.Bool (Bool (False, True), (&&))
import "base" Data.Coerce (Coercible, coerce)
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Either qualified as Base (either)
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Function (const, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Functor.Compose (Compose (Compose))
import safe "base" Data.Functor.Const (Const (Const))
import safe "base" Data.Functor.Product (Product (Pair))
import safe "base" Data.Maybe (Maybe, maybe)
import safe "base" Data.Ord (Ord, (<=))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (uncurry)
import safe "base" Data.Void (Void)
import safe "base" GHC.Natural (naturalToInteger)
import safe "base" Text.Show (Show, show)
import safe "containers" Data.Map (Map)
import "dhall" Dhall qualified (Decoder (Decoder), RecordDecoder (RecordDecoder))
import "dhall" Dhall qualified as Dhall.Decode
import "dhall" Dhall.Src qualified as Dhall (Src)
import "either" Data.Either.Validation qualified as Validation
import safe "hashable" Data.Hashable (Hashable)
import safe "indexed-traversable" Data.Foldable.WithIndex (ifoldMap)
import safe "scientific" Data.Scientific (Scientific)
import safe "semigroupoids" Data.Functor.Alt ((<!>))
import safe "text" Data.Text (Text)
import safe "text" Data.Text qualified as Text
import safe "unordered-containers" Data.HashMap.Strict (HashMap)
import "vector" Data.Vector (Vector)
import safe "base" Prelude (Integer, undefined)

null :: (Coercible output ()) => Dhall.Decoder output
null = coerce <$> Dhall.Decode.unit

bool :: (Coercible output Bool) => Dhall.Decoder output
bool = coerce <$> Dhall.Decode.bool

string :: (Coercible output Text) => Dhall.Decoder output
string = coerce <$> Dhall.Decode.strictText

integer ::
  (Coercible output Integer) => Autodo.Bounds Integer -> Dhall.Decoder output
integer (Autodo.Bounds lower upper) =
  fmap coerce
    . bindExtractor
      ( \i ->
          if maybe True (<= i) lower && maybe True (i <=) upper
            then Base.pure i
            else Dhall.Decode.extractError "out of range"
      )
    $ if maybe False (0 <=) lower
      then naturalToInteger <$> Dhall.Decode.natural
      else Dhall.Decode.integer

number :: (Coercible output Scientific) => Dhall.Decoder output
number = coerce <$> Dhall.Decode.scientific

hashMap ::
  (Hashable k, FromJSONKey k, Coercible output (HashMap k v)) =>
  Dhall.Decoder v ->
  Dhall.Decoder output
hashMap =
  coerce
    <$> Dhall.Decode.hashMap
      ( ( case fromJSONKey of
            FromJSONKeyCoerce -> coerce
            FromJSONKeyText f -> f
            FromJSONKeyTextParser _tp -> undefined
            FromJSONKeyValue _vp -> undefined
        )
          <$> Dhall.Decode.strictText
      )

map :: (Ord k, FromJSONKey k, Coercible output (Map k v)) => Dhall.Decoder v -> Dhall.Decoder output
map =
  coerce
    <$> Dhall.Decode.map
      ( ( case fromJSONKey of
            FromJSONKeyCoerce -> coerce
            FromJSONKeyText f -> f
            FromJSONKeyTextParser _tp -> undefined
            FromJSONKeyValue _vp -> undefined
        )
          <$> Dhall.Decode.strictText
      )

value :: (Coercible JSON.Value output) => Dhall.Decoder output
value =
  coerce
    <$> ( Dhall.Decoder
            { extract = const $ Dhall.Decode.extractError "JSON values aren’t yet supported.",
              expected = Validation.Failure . Dhall.Decode.DhallErrors $ Base.pure Dhall.Decode.RecursiveTypeError
            } ::
            Dhall.Decoder JSON.Value
        )

arrayOf :: (Coercible output (Vector output1)) => Dhall.Decoder output1 -> Dhall.Decoder output
arrayOf = fmap coerce . Dhall.Decode.vector

objectOf :: Dhall.RecordDecoder output -> Dhall.Decoder output
objectOf = Dhall.Decode.record

eq ::
  (Show val, Eq val, Coercible output val) =>
  val ->
  Dhall.Decoder val ->
  Dhall.Decoder output
eq expectedValue =
  bindExtractor
    ( \actual ->
        if expectedValue == actual
          then Base.pure $ coerce actual
          else
            Dhall.Decode.extractError $
              "Expected "
                <> Text.pack (show expectedValue)
                <> ", but got "
                <> Text.pack (show actual)
    )

bimap :: (oldOutput -> Either String output) -> Dhall.Decoder oldOutput -> Dhall.Decoder output
bimap f = bindExtractor (Base.either (Dhall.Decode.extractError . Text.pack) Base.pure . f)

bimap' :: (oldOutput -> Either String output) -> Dhall.RecordDecoder oldOutput -> Dhall.RecordDecoder output
bimap' f (Dhall.RecordDecoder (Pair (Const expected) (Compose extract))) =
  Dhall.RecordDecoder . Pair (Const expected) . Compose $
    Dhall.Decode.fromMonadic
      . ( Dhall.Decode.toMonadic
            . Base.either (Dhall.Decode.extractError . Text.pack) Base.pure
            . f
            <=< Dhall.Decode.toMonadic . extract
        )

either ::
  (Coercible output (Either output1 output2)) =>
  Autodo.Union ->
  Dhall.Decoder output1 ->
  Dhall.Decoder output2 ->
  Dhall.Decoder output
either union ld rd =
  coerce
    <$> if union
      == Autodo.DisjointUnion
      && Dhall.Decode.expected ld
        == Dhall.Decode.expected rd
      then
        Dhall.Decoder
          { extract =
              uncurry (<!>)
                . ( fmap Left . Dhall.Decode.extract ld
                      &&& fmap Base.pure . Dhall.Decode.extract rd
                  ),
            expected = Dhall.Decode.expected ld
          }
      else
        Dhall.Decode.union $
          Dhall.Decode.constructor "Left" (Left <$> ld)
            <> Dhall.Decode.constructor "Right" (Base.pure <$> rd)

discriminatedUnion ::
  Text ->
  HashMap Autodo.Discriminator (Dhall.RecordDecoder output) ->
  Dhall.RecordDecoder output
discriminatedUnion property =
  Dhall.Decode.field property
    . Dhall.Decode.union
    . ifoldMap (\k -> Dhall.Decode.constructor k . Dhall.Decode.record)

comment :: Text -> Dhall.Decoder output -> Dhall.Decoder output
comment _comment = id

reference :: Text -> Dhall.Decoder output -> Dhall.Decoder output
reference _name = id

requiredKey ::
  (Coercible output output1) =>
  Text ->
  Dhall.Decoder output1 ->
  Dhall.RecordDecoder output
requiredKey key = fmap coerce . Dhall.Decode.field key

optionalKey ::
  (Coercible output (Maybe output1)) =>
  Text ->
  Dhall.Decoder output1 ->
  Dhall.RecordDecoder output
optionalKey key = fmap coerce . parseOptionalField key

optionalKeyWithDefault ::
  (Coercible output input) =>
  Text ->
  Dhall.Decoder input ->
  input ->
  Dhall.RecordDecoder output
optionalKeyWithDefault key v def = maybe (coerce def) coerce <$> parseOptionalField key v

optionalKeyWithOmittedDefault ::
  (Coercible output val) =>
  Text ->
  Dhall.Decoder val ->
  val ->
  Dhall.RecordDecoder output
optionalKeyWithOmittedDefault k v def =
  maybe (coerce def) coerce <$> parseOptionalField k v

pure :: output -> Dhall.RecordDecoder output
pure a = Base.pure a

ap ::
  Dhall.RecordDecoder (output1 -> output) ->
  Dhall.RecordDecoder output1 ->
  Dhall.RecordDecoder output
ap = (<*>)

-- | Like `=<<` for `Dhall.Decoder`.
bindExtractor ::
  (a -> Dhall.Decode.Extractor Dhall.Src Void b) ->
  Dhall.Decoder a ->
  Dhall.Decoder b
bindExtractor fn Dhall.Decoder {extract, expected} =
  Dhall.Decoder
    { extract =
        Dhall.Decode.fromMonadic
          . ( Dhall.Decode.toMonadic . fn
                <=< Dhall.Decode.toMonadic . extract
            ),
      expected
    }

parseOptionalField :: Text -> Dhall.Decoder a -> Dhall.RecordDecoder (Maybe a)
parseOptionalField k = Dhall.Decode.field k . Dhall.Decode.maybe
