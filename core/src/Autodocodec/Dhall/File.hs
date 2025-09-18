{-# LANGUAGE Trustworthy #-}

module Autodocodec.Dhall.File
  ( read,
    readFirst,
  )
where

import "autodocodec" Autodocodec qualified as Autodo
import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((=<<))
import safe "base" Data.Function (($))
import safe "base" Data.Maybe (Maybe (Nothing), maybe)
import safe "base" System.IO (IO)
import "dhall" Dhall qualified as Dhall
import "path" Path (File, Path, toFilePath)
import "path-io" Path.IO (forgivingAbsence)
import safe "this" Autodocodec.Dhall qualified as Autodo.Dhall

-- | Helper function to read a yaml file for a type in 'HasCodec'
--
-- This will output a colourful yaml schema if parsing fails.
read :: (Autodo.HasCodec a) => Path r File -> IO (Maybe a)
read = readFirst . pure

-- | Helper function to read the first in a list of yaml files for a type is 'HasCodec'
--
-- This will output a colourful yaml schema if parsing fails.
readFirst :: forall a r. (Autodo.HasCodec a) => [Path r File] -> IO (Maybe a)
readFirst files = go files
  where
    go' :: Path r File -> IO (Maybe a)
    go' p =
      forgivingAbsence
        -- . either
        --   ( \err ->
        --       die . unlines $
        --         [ "Failed to parse Dhall config file",
        --           toFilePath p,
        --           "with error:",
        --           Yaml.prettyPrintParseException err
        --         ]
        --           <> ( case files of
        --                  [] -> []
        --                  [_] -> []
        --                  fs ->
        --                    "while parsing files:"
        --                      : map (("• " <>) . toFilePath) fs
        --              )
        --           <> [ "Reference: ",
        --                T.unpack $ Autodo.Dhall.renderColouredSchemaViaCodec @a
        --              ]
        --   )
        --   pure
        . Dhall.inputFile Autodo.Dhall.decodeViaCodec
        $ toFilePath p

    go :: [Path r File] -> IO (Maybe a)
    go = \case
      [] -> pure Nothing
      p : ps -> maybe (go ps) (pure . pure) =<< go' p
