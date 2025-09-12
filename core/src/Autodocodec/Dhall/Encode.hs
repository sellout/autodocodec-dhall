{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Autodocodec.Dhall.Encode
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
  ( ToJSONKey,
    ToJSONKeyFunction (ToJSONKeyText, ToJSONKeyValue),
    toJSONKey,
  )
import "aeson" Data.Aeson qualified as JSON
import "aeson" Data.Aeson.Encoding (encodingToLazyByteString)
import "autodocodec" Autodocodec.Codec qualified as Autodo
import safe "base" Control.Applicative qualified as Base (pure)
import safe "base" Control.Category (id, (.))
import safe "base" Data.Bool (Bool (False), (&&))
import safe "base" Data.Bool qualified as Base (bool)
import "base" Data.Coerce (Coercible, coerce)
import safe "base" Data.Either (Either)
import safe "base" Data.Either qualified as Base (either)
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Foldable (foldl')
import safe "base" Data.Foldable qualified as Base (null)
import safe "base" Data.Function (const, flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Functor.Contravariant (Contravariant, contramap, (>$<))
import safe "base" Data.List.NonEmpty (NonEmpty ((:|)), nonEmpty)
import safe "base" Data.Maybe (Maybe (Nothing), maybe)
import safe "base" Data.Ord ((<=))
import safe "base" Data.Tuple (uncurry)
import safe "base" Data.Void (Void, absurd)
import "base" GHC.Num (integerToNaturalClamp)
import safe "containers" Data.Map (Map)
import safe "containers" Data.Map qualified as Map
import safe "containers" Data.Sequence qualified as Seq
import safe "contravariant" Data.Functor.Contravariant.Divisible (conquer)
import "dhall" Dhall.Core qualified as Dhall
  ( Expr (App, Field, List, ListLit, None, Optional, Record, RecordLit, Some),
    makeFieldSelection,
    makeRecordField,
  )
import "dhall" Dhall.Map qualified
import "dhall" Dhall.Marshal.Encode ((>|<))
import "dhall" Dhall.Marshal.Encode qualified as Dhall
import safe "indexed-traversable" Data.Foldable.WithIndex (itoList)
import safe "scientific" Data.Scientific (Scientific)
import safe "text" Data.Text (Text)
import safe "unordered-containers" Data.HashMap.Strict (HashMap)
import safe "unordered-containers" Data.HashMap.Strict qualified as HashMap
import "vector" Data.Vector (Vector)
import "vector" Data.Vector qualified as Vector
import safe "base" Prelude (Integer, undefined)

-- $setup
-- >>> :set -XOverloadedLists
-- >>> :set -XOverloadedStrings
-- >>> import "base" Data.Bool (Bool (False, True))
-- >>> import "base" Data.Either (Either (Left, Right))
-- >>> import "base" Data.Eq (Eq)
-- >>> import "base" Data.Int (Int8)
-- >>> import "base" Text.Show (Show)
-- >>> import "base" Prelude (Bounded, Enum)

encodeMaybe :: Dhall.Encoder a -> Dhall.Encoder (Maybe a)
encodeMaybe (Dhall.Encoder elemEmbed elemDeclared) =
  Dhall.Encoder
    { embed =
        maybe (Dhall.App Dhall.None elemDeclared) (Dhall.Some . elemEmbed),
      declared = Dhall.App Dhall.Optional elemDeclared
    }

encodeEither :: Dhall.Encoder l -> Dhall.Encoder r -> Dhall.Encoder (Either l r)
encodeEither l r =
  Dhall.unionEncoder $
    -- TODO: In an object context, this is where we should decide whether to
    --       “inline” single-field records, using their field names instead of
    --       @Left@ and @Right@. Unlike the other cases, this one might also
    --       depend on whether /both/ alternatives are single-field records (so
    --       we don’t end up with @Left@ and @myFieldName@).
    --
    --       We might also want to simplify cases with an empty branch to
    --       @Optional@.
    Dhall.encodeConstructorWith "Left" l
      >|< Dhall.encodeConstructorWith "Right" r

encodeSeq :: Dhall.Encoder a -> Dhall.Encoder (Seq.Seq a)
encodeSeq (Dhall.Encoder embedIn declaredIn) =
  let declared = Dhall.App Dhall.List declaredIn
   in Dhall.Encoder
        { embed = \xs ->
            Dhall.ListLit (Base.bool Nothing (Base.pure declared) $ Seq.null xs) $
              embedIn <$> xs,
          declared
        }

encodeMap :: Dhall.Encoder k -> Dhall.Encoder v -> Dhall.Encoder [(k, v)]
encodeMap (Dhall.Encoder embedK declaredK) (Dhall.Encoder embedV declaredV) =
  let declared =
        Dhall.App Dhall.List . Dhall.Record $ recordPair declaredK declaredV
   in Dhall.Encoder
        { embed = \m ->
            Dhall.ListLit (Base.bool Nothing (Base.pure declared) $ Base.null m)
              . Seq.fromList
              $ uncurry (\k -> Dhall.RecordLit . recordPair (embedK k) . embedV)
                <$> m,
          declared
        }
  where
    recordPair k v =
      Dhall.Map.fromList
        [ ("mapKey", Dhall.makeRecordField k),
          ("mapValue", Dhall.makeRecordField v)
        ]

encodeJSONValue :: Dhall.Encoder JSON.Value
encodeJSONValue = undefined

-- | @{}@
null :: (Coercible input ()) => Dhall.Encoder input
null = coerce >$< (Dhall.inject :: Dhall.Encoder ())

-- | @Bool@
bool :: (Coercible input Bool) => Dhall.Encoder input
bool = coerce >$< (Dhall.inject :: Dhall.Encoder Bool)

-- | @Text@
string :: (Coercible input Text) => Dhall.Encoder input
string = coerce >$< (Dhall.inject :: Dhall.Encoder Text)

-- | @Integer@ | @Natural@
integer ::
  (Coercible input Integer) => Autodo.Bounds Integer -> Dhall.Encoder input
integer (Autodo.Bounds lower _) =
  if maybe False (0 <=) lower
    then integerToNaturalClamp . coerce >$< Dhall.inject
    else coerce >$< (Dhall.inject :: Dhall.Encoder Integer)

-- | @Double@
number :: (Coercible input Scientific) => Dhall.Encoder input
number = coerce >$< (Dhall.inject :: Dhall.Encoder Scientific)

-- | @List { mapKey = k, mapValue = v }@
hashMap ::
  (ToJSONKey k, Coercible input (HashMap k v)) =>
  Dhall.Encoder v ->
  Dhall.Encoder input
hashMap =
  (HashMap.toList . coerce >$<)
    . encodeMap
      ( case toJSONKey of
          ToJSONKeyText _ f -> encodingToLazyByteString . f >$< Dhall.inject
          ToJSONKeyValue f _ -> f >$< encodeJSONValue
      )

-- | @List { mapKey = k, mapValue = v }@
map ::
  (ToJSONKey k, Coercible input (Map k v)) =>
  Dhall.Encoder v ->
  Dhall.Encoder input
map =
  (Map.toList . coerce >$<)
    . encodeMap
      ( case toJSONKey of
          ToJSONKeyText _ f -> encodingToLazyByteString . f >$< Dhall.inject
          ToJSONKeyValue f _ -> f >$< encodeJSONValue
      )

-- | depends on the `valueEncoder` argument
value :: (Coercible JSON.Value input) => Dhall.Encoder input
value = coerce >$< encodeJSONValue

-- | @List v@
arrayOf ::
  (Coercible input (Vector input1)) =>
  Dhall.Encoder input1 ->
  Dhall.Encoder input
arrayOf = (Seq.fromList . Vector.toList . coerce >$<) . encodeSeq

-- |
--
--  __TODO__: This is where we /should/ resolve the user’s simplification
--            preferences – is there only a single field? Does it satisfy
--            whatever properties the user wants to use for simplification? (For
--            example, the body is a discriminated union, or it has a particular
--            name, or particular other type).
--
--   Dhall type: @{ k1 : v1, k2 : v2, … }@
objectOf :: Dhall.RecordEncoder input -> Dhall.Encoder input
objectOf = Dhall.recordEncoder

-- | This always encodes as the provided @value@.
--
--   Dhall type: whatever the input `Autodo.Codec`’s type is.
eq :: val -> Dhall.Encoder val -> Dhall.Encoder input
eq v = (const v >$<)

-- |
--
--   Dhall type: whatever the input `Autodo.Codec`’s type is.
bimap :: (Contravariant f) => (input -> oldInput) -> f oldInput -> f input
bimap g = (g >$<)

-- | Unlike `discriminatedUnion`, this doesn’t give us a field name to wrap in
--   if we’re in an object context. So, we instead build a tree of
--   `Dhall.RecordEncoder` and if any other fields are `ap`ed onto this
--   structure, we duplicate them on each leaf of the tree. This works fine for
--   nested `Either`s, but not for independent `Either`s, which then need to be
--   wrapped in a new `Either` … but that isn’t associative.
--
--  __NB__: It’s tempting to take advantage of the `Autodo.Union` to encode
--          `Autodo.PossiblyJointUnion` as a non-union type when the
--          `Dhall.declared` types coincide, but `Autodo.PossiblyJointUnion`
--          exists precisely to highlight the pitfalls of doing so – without a
--          true disjunction, `Autodo.PossiblyJointUnion` indicates a type that
--          might parse incorrectly, because the domain of the two alternatives
--          overlap. So it’s exactly the cases that we shouldn’t avoid the
--          union.
--
--   Dhall type: @< Left : l | Right : r >@
either ::
  (Coercible input (Either input1 input2)) =>
  -- | The terminology is a little confusing (at least to me), but if this is
  --   `Autodo.DisjointUnion` it means that the values for the two alternatives
  --   are disjoint, so if they are the same type, we can safely elide the union
  --   and represent them as a single value. However,
  --   `Autodo.PossiblyJointUnion` means that some values may be valid for both
  --   alternatives, in which case we need to preserve the union (even if the
  --   alternatives are the same type) to keep them unambiguous.
  Autodo.Union ->
  Dhall.Encoder input1 ->
  Dhall.Encoder input2 ->
  Dhall.Encoder input
either union le re =
  contramap coerce $
    if union == Autodo.DisjointUnion
      && Dhall.declared le == Dhall.declared re
      then
        Dhall.Encoder
          { declared = Dhall.declared le,
            embed = Base.either (Dhall.embed le) (Dhall.embed re)
          }
      else encodeEither le re

-- | Autodocodec has no notion of true unions, so this is encoded as a union
--   wrapped in a record field.
--
--  __TODO__: If this is the only field in a `Dhall.RecordEncoder` when we
--            finish creating an object, we should be able to unwrap it if the
--            user wants us to.
--
--   Dhall type: @{ p : < c1 : v1, c2 : v2, … > }@
discriminatedUnion ::
  Text ->
  (input -> (Autodo.Discriminator, Dhall.RecordEncoder input)) ->
  HashMap Autodo.Discriminator (Dhall.RecordEncoder Void) ->
  Dhall.RecordEncoder input
discriminatedUnion property selector constructors =
  Dhall.encodeFieldWith property $
    let Dhall.Encoder {declared} =
          maybe
            (Dhall.inject :: Dhall.Encoder Void)
            ( \(h :| t) ->
                Dhall.unionEncoder $
                  foldl' (\union cons -> absurd >$< union >|< cons) h t
            )
            . nonEmpty
            . fmap
              (uncurry Dhall.encodeConstructorWith . fmap Dhall.recordEncoder)
            $ itoList constructors
     in Dhall.Encoder
          { embed = \a ->
              uncurry
                (Dhall.App . Dhall.Field declared . Dhall.makeFieldSelection)
                . fmap (flip Dhall.embed a . Dhall.recordEncoder)
                $ selector a,
            declared
          }

-- |
--
--  __TODO__: We should be able to preserve comments in some cases.
--
--   Dhall type: whatever the input `Autodo.Codec`’s type is.
comment :: Text -> Dhall.Encoder input -> Dhall.Encoder input
comment _comment = id

-- |
--
--  __TODO__: This should produce a `Dhall.Var` corresponding to the provided
--            name and return the mapping between that and the Encoder in a
--            `Writer`.
--
--   Dhall type: whatever the input `Autodo.Codec`’s type is.
reference :: Text -> Dhall.Encoder input -> Dhall.Encoder input
reference _name = id

-- |
--
--   Dhall type: @{ k : v }@
requiredKey ::
  (Coercible input input1) =>
  Text ->
  Dhall.Encoder input1 ->
  Dhall.RecordEncoder input
requiredKey key = Dhall.encodeFieldWith key . (coerce >$<)

-- |
--
--   Dhall type: @{ k : Optional v }@
optionalKey ::
  (Coercible input (Maybe input1)) =>
  Text ->
  Dhall.Encoder input1 ->
  Dhall.RecordEncoder input
optionalKey key = Dhall.encodeFieldWith key . contramap coerce . encodeMaybe

-- |
--
--   Dhall type: @{ k : Optional v }@
optionalKeyWithDefault ::
  Text ->
  Dhall.Encoder input ->
  Dhall.RecordEncoder input
optionalKeyWithDefault key =
  Dhall.encodeFieldWith key . contramap Base.pure . encodeMaybe

-- |
--
--   Dhall type: @{ k : Optional v }@
optionalKeyWithOmittedDefault ::
  (Eq val, Coercible input val) =>
  Text ->
  val ->
  Dhall.Encoder val ->
  Dhall.RecordEncoder input
optionalKeyWithOmittedDefault key defaultValue =
  Dhall.encodeFieldWith key
    . ((\v -> if v == defaultValue then Nothing else Base.pure v) . coerce >$<)
    . encodeMaybe

-- |
--
--   Dhall type: @{}@
pure :: Dhall.RecordEncoder input
pure = conquer

-- |
--
--  __FIXME__: This is probably invalid, and will cause a loop.
--
--   Dhall type: @a ⩓ b@
ap ::
  Dhall.RecordEncoder input ->
  Dhall.RecordEncoder input ->
  Dhall.RecordEncoder input
ap (Dhall.RecordEncoder map1) (Dhall.RecordEncoder map2) =
  Dhall.RecordEncoder $ Dhall.Map.union map1 map2
