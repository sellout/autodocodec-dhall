{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Autodocodec.Dhall
  ( declaredObjectVia,
    declaredVia,
    decode,
    decodeObject,
    decodeObjectVia,
    decodeViaCodec,
    embedObjectVia,
    embedVia,
    embedViaCodec,
    encode,
    encodeViaCodec,
    encodeObject,
  )
where

import "autodocodec" Autodocodec.Class qualified as Autodo
import "autodocodec" Autodocodec.Codec qualified as Autodo
import safe "base" Control.Category ((.))
import safe "base" Data.Function (($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Tuple (snd)
import safe "base" Data.Void (Void)
import "dhall" Dhall.Core qualified as Dhall (Expr)
import "dhall" Dhall.Marshal.Decode qualified as Dhall (Decoder, RecordDecoder)
import "dhall" Dhall.Marshal.Decode qualified as Dhall.Decode
import "dhall" Dhall.Marshal.Encode qualified as Dhall
import "dhall" Dhall.Src qualified as Dhall (Src)
import "this" Autodocodec.Dhall.Decode qualified as Decode
import "this" Autodocodec.Dhall.Encode qualified as Encode

decode :: Autodo.ValueCodec void a -> Dhall.Decoder a
decode = \case
  Autodo.NullCodec -> Decode.null
  Autodo.BoolCodec _ -> Decode.bool
  Autodo.StringCodec _ -> Decode.string
  Autodo.IntegerCodec _ bounds -> Decode.integer bounds
  Autodo.NumberCodec _ _ -> Decode.number
  Autodo.HashMapCodec v -> Decode.hashMap $ decode v
  Autodo.MapCodec v -> Decode.map $ decode v
  Autodo.ValueCodec -> Decode.value
  Autodo.ArrayOfCodec _ v -> Decode.arrayOf $ decode v
  Autodo.ObjectOfCodec _ o -> Decode.objectOf $ decodeObject o
  Autodo.EqCodec expectedValue v -> Decode.eq expectedValue $ decode v
  Autodo.BimapCodec f _ c -> Decode.bimap f $ decode c
  Autodo.EitherCodec union l r -> Decode.either union (decode l) $ decode r
  Autodo.CommentCodec comment c -> Decode.comment comment $ decode c
  Autodo.ReferenceCodec name c -> Decode.reference name $ decode c

encode :: Autodo.ValueCodec a void -> Dhall.Encoder a
encode = \case
  Autodo.NullCodec -> Encode.null
  Autodo.BoolCodec _ -> Encode.bool
  Autodo.StringCodec _ -> Encode.string
  Autodo.IntegerCodec _ bounds -> Encode.integer bounds
  Autodo.NumberCodec _ _ -> Encode.number
  Autodo.HashMapCodec c -> Encode.hashMap $ encode c
  Autodo.MapCodec c -> Encode.map $ encode c
  Autodo.ValueCodec -> Encode.value
  Autodo.ArrayOfCodec _ c -> Encode.arrayOf $ encode c
  Autodo.ObjectOfCodec _ oc -> Encode.objectOf $ encodeObject oc
  Autodo.EqCodec v c -> Encode.eq v $ encode c
  Autodo.BimapCodec _ g c -> Encode.bimap g $ encode c
  Autodo.EitherCodec union lc rc -> Encode.either union (encode lc) $ encode rc
  Autodo.CommentCodec comment c -> Encode.comment comment $ encode c
  Autodo.ReferenceCodec name c -> Encode.reference name $ encode c

decodeObject :: Autodo.ObjectCodec void a -> Dhall.RecordDecoder a
decodeObject = \case
  Autodo.BimapCodec f _ c -> Decode.bimap' f $ decodeObject c
  Autodo.EitherCodec union l r ->
    Dhall.Decode.field "undefined"
      . Decode.either union (Dhall.Decode.record $ decodeObject l)
      . Dhall.Decode.record
      $ decodeObject r
  Autodo.DiscriminatedUnionCodec property _ constructors ->
    Decode.discriminatedUnion property $ decodeObject . snd <$> constructors
  Autodo.RequiredKeyCodec k v _ -> Decode.requiredKey k $ decode v
  Autodo.OptionalKeyCodec k v _ -> Decode.optionalKey k $ decode v
  Autodo.OptionalKeyWithDefaultCodec k v def _ ->
    Decode.optionalKeyWithDefault k (decode v) def
  Autodo.OptionalKeyWithOmittedDefaultCodec k v def _ ->
    Decode.optionalKeyWithOmittedDefault k (decode v) def
  Autodo.PureCodec a -> Decode.pure a
  Autodo.ApCodec f a -> Decode.ap (decodeObject f) $ decodeObject a

encodeObject :: Autodo.ObjectCodec a void -> Dhall.RecordEncoder a
encodeObject = \case
  Autodo.BimapCodec _ g c -> Encode.bimap g $ encodeObject c
  Autodo.EitherCodec union lc rc ->
    -- FIXME: This currently creates a not-very-good field to use to shove this
    --        into a record, but this is not the correct way to do this.
    Dhall.encodeFieldWith "undefined"
      . Encode.either union (Dhall.recordEncoder $ encodeObject lc)
      . Dhall.recordEncoder
      $ encodeObject rc
  Autodo.DiscriminatedUnionCodec propertyName selector constructors ->
    Encode.discriminatedUnion propertyName (fmap (encodeObject <$>) selector) $
      fmap (encodeObject . snd) constructors
  Autodo.RequiredKeyCodec k c _ -> Encode.requiredKey k $ encode c
  Autodo.OptionalKeyCodec k c _ -> Encode.optionalKey k $ encode c
  Autodo.OptionalKeyWithDefaultCodec k c _ _ ->
    Encode.optionalKeyWithDefault k $ encode c
  Autodo.OptionalKeyWithOmittedDefaultCodec k c defaultValue _ ->
    Encode.optionalKeyWithOmittedDefault k defaultValue $ encode c
  Autodo.PureCodec _ -> Encode.pure
  Autodo.ApCodec oc1 oc2 -> Encode.ap (encodeObject oc1) $ encodeObject oc2

-- | Embed a value into a `Dhall.Expr` using the provided codec.
embedVia :: Autodo.ValueCodec a void -> a -> Dhall.Expr Dhall.Src Void
embedVia = Dhall.embed . encode

encodeViaCodec :: (Autodo.HasCodec a) => Dhall.Encoder a
encodeViaCodec = encode Autodo.codec

embedViaCodec :: (Autodo.HasCodec a) => a -> Dhall.Expr Dhall.Src Void
embedViaCodec = embedVia Autodo.codec

-- | Returns the Dhall type corresponding to a codec.
declaredVia :: Autodo.ValueCodec a void -> Dhall.Expr Dhall.Src Void
declaredVia = Dhall.declared . encode

embedObjectVia :: Autodo.ObjectCodec a void -> a -> Dhall.Expr Dhall.Src Void
embedObjectVia = Dhall.embed . Dhall.recordEncoder . encodeObject

declaredObjectVia :: Autodo.ObjectCodec a void -> Dhall.Expr Dhall.Src Void
declaredObjectVia = Dhall.declared . Dhall.recordEncoder . encodeObject

decodeViaCodec :: (Autodo.HasCodec a) => Dhall.Decoder a
decodeViaCodec = decode Autodo.codec

decodeObjectVia :: Autodo.ObjectCodec void a -> Dhall.Decoder a
decodeObjectVia = Dhall.Decode.record . decodeObject
