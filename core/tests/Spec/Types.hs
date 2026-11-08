{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

-- |
-- Copyright: 2025 Greg Pfeil
-- License: AGPL-3.0-only WITH Universal-FOSS-exception-1.0 OR LicenseRef-proprietary
--
-- @since 99999
module Spec.Types
  ( Ainur (Maiar, Valar),
    Fruit (Apple, Orange),
    War (OtherWar, WorldWar),
  )
where

import "autodocodec" Autodocodec.Class qualified as Autodo
import "autodocodec" Autodocodec.Codec ((.=))
import "autodocodec" Autodocodec.Codec qualified as Autodo
import safe "base" Control.Applicative (pure, (<*>))
import safe "base" Control.Category ((.))
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Eq (Eq)
import safe "base" Data.Function (($))
import safe "base" Data.Functor ((<$>))
import safe "base" Data.Kind (Type)
import safe "base" Data.Tuple (fst, snd, uncurry)
import safe "base" Data.Word (Word8)
import safe "base" Text.Show (Show)
import safe "text" Data.Text (Text)
import safe "base" Prelude (Bounded, Enum)

-- | A simple enumeration.
--
-- @since 99999
type Fruit :: Type
data Fruit
  = -- |
    --
    -- @since 99999
    Apple
  | -- |
    --
    -- @since 99999
    Orange
  deriving stock
    ( -- | @since 99999
      Bounded,
      -- | @since 99999
      Enum,
      -- | @since 99999
      Eq,
      -- | @since 99999
      Show
    )

-- | A disjoint union of two types.
--
-- @since 99999
type War :: Type
data War
  = -- |
    --
    -- @since 99999
    WorldWar Word8
  | -- |
    --
    -- @since 99999
    OtherWar Text
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Show
    )

-- | @since 99999
instance Autodo.HasCodec War where
  codec =
    Autodo.dimapCodec f g $
      Autodo.disjointEitherCodec
        (Autodo.codec :: Autodo.JSONCodec Word8)
        (Autodo.codec :: Autodo.JSONCodec Text)
    where
      f = either WorldWar OtherWar
      g = \case
        WorldWar w -> Left w
        OtherWar t -> pure t

-- | A possibly-joint union of two objects.
--
-- @since 99999
type Ainur :: Type
data Ainur
  = -- |
    --
    -- @since 99999
    Valar Text Text
  | -- |
    --
    -- @since 99999
    Maiar Text
  deriving stock
    ( -- | @since 99999
      Eq,
      -- | @since 99999
      Show
    )

-- | @since 99999
instance Autodo.HasCodec Ainur where
  codec =
    Autodo.dimapCodec f g
      . Autodo.possiblyJointEitherCodec
        ( Autodo.object "Valar" $
            (,)
              <$> Autodo.requiredField
                "domain"
                "Domain which the Valar rules over"
                .= fst
              <*> Autodo.requiredField "name" "Name of the Valar" .= snd
        )
      . Autodo.object "Maiar"
      $ Autodo.requiredField "name" "Name of the Maiar"
    where
      f = either (uncurry Valar) Maiar
      g = \case
        Valar domain name -> Left (domain, name)
        Maiar name -> pure name
