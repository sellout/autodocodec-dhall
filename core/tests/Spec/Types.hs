{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Unsafe #-}

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

type Fruit :: Type
data Fruit = Apple | Orange deriving stock (Show, Eq, Bounded, Enum)

type War :: Type
data War = WorldWar Word8 | OtherWar Text deriving stock (Show, Eq)

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

type Ainur :: Type
data Ainur
  = Valar Text Text
  | Maiar Text
  deriving stock (Show, Eq)

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
