{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}
{-# OPTIONS_GHC -Wwarn=incomplete-patterns #-}

module Autodocodec.Dhall.Decode
  ( parseViaCodec,
    parseVia,
  )
where

import qualified "autodocodec" Autodocodec as Autodo
import safe "base" Control.Applicative
  ( pure, -- (<*>)
  )
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((<=<))
import "base" Data.Coerce (coerce)
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Eq ((==))
import safe "base" Data.Function (const, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Maybe (maybe)
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.Tuple (snd)
import safe "base" Text.Show (show)
import qualified "dhall" Dhall.Marshal.Decode as Dhall
  ( Decoder (Decoder),
    RecordDecoder,
    UnionDecoder,
  )
import qualified "dhall" Dhall.Marshal.Decode as Dhall.Decode
import qualified "either" Data.Either.Validation as Validation
import safe qualified "text" Data.Text as Text
import safe qualified "unordered-containers" Data.HashMap.Strict as HashMap

parseViaCodec :: (Autodo.HasCodec a) => Dhall.Decoder a
parseViaCodec = parseVia Autodo.codec

parseVia :: Autodo.ValueCodec void a -> Dhall.Decoder a
parseVia = \case
  Autodo.NullCodec -> coerce <$> Dhall.Decode.unit
  Autodo.BoolCodec _ -> coerce <$> Dhall.Decode.bool
  Autodo.StringCodec _ -> coerce <$> Dhall.Decode.strictText
  Autodo.IntegerCodec _ _ -> coerce <$> Dhall.Decode.integer
  Autodo.NumberCodec _ _ -> coerce <$> Dhall.Decode.scientific
  -- Autodo.HashMapCodec v -> coerce <$> Dhall.Decode.hashMap (Compat.toKey <$> Dhall.Decode.strictText) (parseVia v)
  -- Autodo.MapCodec v -> coerce <$> Dhall.Decode.map (Dhall.Decode.strictText) (parseVia v)
  Autodo.ValueCodec ->
    Dhall.Decoder
      { extract = const $ Dhall.Decode.extractError "JSON values aren’t yet supported.",
        expected = Validation.Failure . Dhall.Decode.DhallErrors $ pure Dhall.Decode.RecursiveTypeError
      }
  Autodo.ArrayOfCodec _ v -> coerce <$> Dhall.Decode.vector (parseVia v)
  Autodo.ObjectOfCodec _ o -> parseObjectVia o
  Autodo.EqCodec expectedValue v ->
    let Dhall.Decoder {extract, expected} = parseVia v
     in Dhall.Decoder
          { extract =
              Dhall.Decode.fromMonadic
                . ( ( \actual ->
                        Dhall.Decode.toMonadic $
                          if expectedValue == actual
                            then pure $ coerce actual
                            else
                              Dhall.Decode.extractError $
                                "Expected "
                                  <> Text.pack (show expectedValue)
                                  <> ", but got "
                                  <> Text.pack (show actual)
                    )
                      <=< Dhall.Decode.toMonadic . extract
                  ),
            expected
          }
  Autodo.BimapCodec f _ c ->
    let Dhall.Decoder {extract, expected} = parseVia c
     in Dhall.Decoder
          { extract = Dhall.Decode.fromMonadic . (Dhall.Decode.toMonadic . either (Dhall.Decode.extractError . Text.pack) pure . f <=< Dhall.Decode.toMonadic . extract),
            expected
          }
  Autodo.EitherCodec _ l r ->
    fmap coerce Dhall.Decode.union $
      Dhall.Decode.constructor "Left" (Left <$> parseVia l)
        <> Dhall.Decode.constructor "Right" (pure <$> parseVia r)
  Autodo.CommentCodec _ c -> coerce <$> parseVia c
  Autodo.ReferenceCodec _ c -> coerce <$> parseVia c

parseObjectVia :: Autodo.ObjectCodec void a -> Dhall.Decoder a
parseObjectVia = either Dhall.Decode.union Dhall.Decode.record . parseObjectVia'

parseObjectVia' ::
  Autodo.ObjectCodec void a ->
  Either (Dhall.UnionDecoder a) (Dhall.RecordDecoder a)
parseObjectVia' = \case
  -- Autodo.BimapCodec f _ c ->
  --   let Dhall.Decoder {extract, expected} = parseObjectVia c
  --    in Dhall.Decoder
  --         { extract = Dhall.Decode.fromMonadic . (Dhall.Decode.toMonadic . either (Dhall.Decode.extractError . Text.pack) pure . f <=< Dhall.Decode.toMonadic . extract),
  --           expected
  --         }
  Autodo.EitherCodec _ l r ->
    Left . fmap coerce $
      Dhall.Decode.constructor "Left" (Left <$> parseObjectVia l)
        <> Dhall.Decode.constructor "Right" (pure <$> parseObjectVia r)
  Autodo.DiscriminatedUnionCodec _ _ u ->
    Left . fmap coerce $
      HashMap.foldMapWithKey (\k -> Dhall.Decode.constructor k . parseObjectVia . snd) u
  Autodo.RequiredKeyCodec k v _ -> pure . fmap coerce . Dhall.Decode.field k $ parseVia v
  Autodo.OptionalKeyCodec k v _ -> pure $ coerce <$> parseOptionalField k v
  Autodo.OptionalKeyWithDefaultCodec k v def _ ->
    pure $ maybe (coerce def) coerce <$> parseOptionalField k v
  Autodo.OptionalKeyWithOmittedDefaultCodec k v def _ ->
    pure $ maybe (coerce def) coerce <$> parseOptionalField k v
  Autodo.PureCodec a -> pure $ pure a
  -- Autodo.ApCodec f a -> pure $ parseObjectVia f <*> parseObjectVia a
  where
    parseOptionalField k = Dhall.Decode.field k . Dhall.Decode.maybe . parseVia
