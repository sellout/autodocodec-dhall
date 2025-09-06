{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures -Wwarn=incomplete-patterns #-}

module Autodocodec.Dhall.Encode
  ( toDhallTypeVia,
    toDhallVia,
  )
where

import qualified "aeson" Data.Aeson as JSON
import qualified "autodocodec" Autodocodec.Aeson.Compat as Compat
import qualified "autodocodec" Autodocodec.Codec as Autodo
import safe "base" Control.Applicative (pure, (<*>))
import safe "base" Control.Category (id, (.))
import safe "base" Data.Bifunctor (bimap)
import safe "base" Data.Bool (Bool (False, True), bool)
import "base" Data.Coerce (coerce)
import safe "base" Data.Either (Either (Left), either)
import safe "base" Data.Eq ((==))
import safe "base" Data.Foldable (foldMap)
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Functor (fmap, (<$>))
import safe "base" Data.Maybe (Maybe (Nothing), fromMaybe, maybe)
import safe "base" Data.Monoid (mempty)
import safe "base" Data.Ord ((<=))
import safe "base" Data.Semigroup ((<>))
import safe "base" Data.Tuple (snd, uncurry)
import safe "base" GHC.Num (integerToNaturalClamp)
import qualified "dhall" Dhall.Core as Dhall
import "dhall" Dhall.Map (Map)
import qualified "dhall" Dhall.Map as Map
import safe "indexed-traversable" Data.Foldable.WithIndex (ifoldMap)
import safe qualified "scientific" Data.Scientific as Scientific
import safe "text" Data.Text (Text)
import safe qualified "unordered-containers" Data.HashMap.Strict as HashMap
import "vector" Data.Vector (Vector)

-- | This is like `toDhallTypeVia`, but it returns `Nothing` for `NullCodec`, so
--   it can be elided for unions.
toDhallTypeVia' :: Autodo.ValueCodec a void -> Maybe (Dhall.Expr s a')
toDhallTypeVia' = \case
  -- @{}@
  Autodo.NullCodec -> Nothing
  -- @Bool@
  Autodo.BoolCodec _ -> pure Dhall.Bool
  -- @Text@
  Autodo.StringCodec _ -> pure Dhall.Text
  -- @Integer@ | @Natural@
  Autodo.IntegerCodec _ (Autodo.Bounds lower _) ->
    pure $ if maybe False (0 <=) lower then Dhall.Natural else Dhall.Integer
  -- @Double@
  Autodo.NumberCodec _ _ -> pure Dhall.Double
  -- @List {mapKey = k, mapValue = v}@
  Autodo.HashMapCodec c -> pure $ makeMapType c
  -- @List {mapKey : k, mapValue : v}@
  Autodo.MapCodec c -> pure $ makeMapType c
  -- @List v@
  Autodo.ArrayOfCodec _ c -> pure . Dhall.App Dhall.List $ toDhallTypeVia c
  -- @<c1 : v1, c2 : v2, …>@ | @{k1 : v1, k2 : v2, …}@
  Autodo.ObjectOfCodec _ oc -> pure $ toDhallObjectTypeVia oc
  -- no effect
  Autodo.EqCodec _ c -> toDhallTypeVia' c
  -- no effect
  Autodo.BimapCodec _ _ c -> toDhallTypeVia' c
  -- @<Left : l | Right : r>@
  Autodo.EitherCodec _ lc rc ->
    pure . Dhall.Union $
      Map.fromList [("Left", toDhallTypeVia' lc), ("Right", toDhallTypeVia' rc)]
  -- no effect
  Autodo.CommentCodec _ c -> toDhallTypeVia' c
  -- no effect
  Autodo.ReferenceCodec _ c -> toDhallTypeVia' c

toDhallObjectTypeVia :: Autodo.ObjectCodec a void -> Dhall.Expr s a'
toDhallObjectTypeVia =
  uncurry ($)
    . bimap
      ( bool
          (Dhall.Union . fmap pure)
          (Dhall.Record . fmap Dhall.makeRecordField)
      )
      Map.fromList
    . dhallObjectType'
  where
    -- The `Bool` returned indicates whether the result represents a
    -- `Dhall.Record` (`True`) or a `Dhall.Union` (`False`). This is because
    -- "Autodocodec" models unions in the style of C unions.
    dhallObjectType' :: Autodo.ObjectCodec a void -> (Bool, [(Text, Dhall.Expr s a')])
    dhallObjectType' = \case
      -- @{k : v}@
      Autodo.RequiredKeyCodec k c _ -> (True, [(k, toDhallTypeVia c)])
      -- @{k : Optional v}@
      Autodo.OptionalKeyCodec k c _ ->
        (True, [(k, Dhall.App Dhall.Optional $ toDhallTypeVia c)])
      -- @{k : Optional v}@
      Autodo.OptionalKeyWithDefaultCodec k c _ _ ->
        (True, [(k, Dhall.App Dhall.Optional $ toDhallTypeVia c)])
      -- @{k : Optional v}@
      Autodo.OptionalKeyWithOmittedDefaultCodec k c _ _ ->
        (True, [(k, Dhall.App Dhall.Optional $ toDhallTypeVia c)])
      -- no effect
      Autodo.BimapCodec _ _ c -> dhallObjectType' c
      -- @<Left : l | Right : r>@
      Autodo.EitherCodec _ lc rc ->
        (False, [("Left", toDhallObjectTypeVia lc), ("Right", toDhallObjectTypeVia rc)])
      -- @<c1 : v1, c2 : v2, …>@
      Autodo.DiscriminatedUnionCodec _ _ m ->
        (False, HashMap.toList $ toDhallObjectTypeVia . snd <$> m)
      -- @{}@
      Autodo.PureCodec _ -> (True, [])
      Autodo.ApCodec oc1 oc2 ->
        (True, snd (dhallObjectType' oc1) <> snd (dhallObjectType' oc2))

toDhallTypeVia :: Autodo.ValueCodec a void -> Dhall.Expr s a'
toDhallTypeVia = fromMaybe (Dhall.Record Map.empty) . toDhallTypeVia'

toDhallVia :: Autodo.ValueCodec a void -> a -> Dhall.Expr s a'
toDhallVia = flip go
  where
    go :: a -> Autodo.ValueCodec a void -> Dhall.Expr s a'
    go a = \case
      -- @{=}@
      Autodo.NullCodec -> Dhall.RecordLit mempty
      -- @True@ | @False@
      Autodo.BoolCodec _ -> Dhall.BoolLit $ coerce a
      Autodo.StringCodec _ -> Dhall.TextLit . Dhall.Chunks [] $ coerce a
      Autodo.IntegerCodec _ (Autodo.Bounds lower _) ->
        ( if maybe False (0 <=) lower
            then Dhall.IntegerLit
            else Dhall.NaturalLit . integerToNaturalClamp
        )
          $ coerce a
      Autodo.NumberCodec _ _ ->
        Dhall.DoubleLit . Dhall.DhallDouble . Scientific.toRealFloat $ coerce a
      -- Autodo.HashMapCodec c -> makeMap a c
      -- Autodo.MapCodec c -> makeMap a c
      Autodo.ValueCodec -> dhallValue $ coerce a
      Autodo.ArrayOfCodec _ c ->
        Dhall.ListLit (pure $ toDhallTypeVia c)
          . foldMap @Vector (pure . (`go` c))
          $ coerce a
      Autodo.ObjectOfCodec _ oc -> goObject a oc
      Autodo.EqCodec value c -> go value c
      Autodo.BimapCodec _ g c -> go (g a) c
      ec@(Autodo.EitherCodec _ lc rc) ->
        let field = Dhall.Field (toDhallTypeVia ec) . Dhall.makeFieldSelection
         in either
              (Dhall.App (field "Left") . (`go` lc))
              (Dhall.App (field "Right") . (`go` rc))
              $ coerce a
      Autodo.CommentCodec _ c -> go a c
      Autodo.ReferenceCodec _ c -> go a c
    goObject a =
      either id (Dhall.RecordLit . fmap Dhall.makeRecordField . Map.fromList)
        . goObject' a
    -- This will return `Either` a `Dhall.Union` or the fields to build a
    -- `Dhall.Record`. This is because the "Autodocodec" representation of
    -- unions is like C union.
    goObject' ::
      a ->
      Autodo.ObjectCodec a void ->
      Either (Dhall.Expr s a') [(Text, Dhall.Expr s a')]
    goObject' a = \case
      Autodo.RequiredKeyCodec k c _ -> pure [(k, go (coerce a) c)]
      Autodo.OptionalKeyCodec k c _ ->
        pure
          [ ( k,
              maybe
                ( Dhall.Annot Dhall.None . Dhall.App Dhall.Optional $
                    toDhallTypeVia c
                )
                (Dhall.Some . (`go` c))
                $ coerce a
            )
          ]
      Autodo.OptionalKeyWithDefaultCodec k c _ _ -> pure [(k, go (coerce a) c)]
      Autodo.OptionalKeyWithOmittedDefaultCodec k c defaultValue _ ->
        pure
          [ ( k,
              let value = coerce a
               in if value == defaultValue
                    then
                      Dhall.Annot Dhall.None . Dhall.App Dhall.Optional $
                        toDhallTypeVia c
                    else Dhall.Some $ go value c
            )
          ]
      Autodo.BimapCodec _ g c -> goObject' (g a) c
      ec@(Autodo.EitherCodec _ lc rc) ->
        let field =
              Dhall.Field (toDhallObjectTypeVia ec) . Dhall.makeFieldSelection
         in Left
              . either
                (Dhall.App (field "Left") . (`goObject` lc))
                (Dhall.App (field "Right") . (`goObject` rc))
              $ coerce a
      uc@(Autodo.DiscriminatedUnionCodec fieldName s _) ->
        Left
          . Dhall.App
            ( Dhall.Field (toDhallObjectTypeVia uc) $
                Dhall.makeFieldSelection fieldName
            )
          . goObject a
          . snd
          $ s a
      Autodo.PureCodec _ -> pure []
      Autodo.ApCodec oc1 oc2 -> (<>) <$> goObject' a oc1 <*> goObject' a oc2
    -- TODO: This should be wrapped in a fixed-point of JSON, like I’ve defined
    --       … somewhere.
    dhallValue = \case
      JSON.Null -> Dhall.RecordLit mempty
      JSON.Bool b -> Dhall.BoolLit b
      JSON.String s -> Dhall.TextLit $ Dhall.Chunks [] s
      JSON.Number n ->
        Dhall.DoubleLit . Dhall.DhallDouble $ Scientific.toRealFloat n
      JSON.Object o ->
        Dhall.RecordLit $
          ifoldMap
            ( \k ->
                Map.singleton (Compat.fromKey k)
                  . Dhall.makeRecordField
                  . dhallValue
            )
            o
      JSON.Array v -> Dhall.ListLit Nothing $ foldMap (pure . dhallValue) v

-- makeMap a c =
--   Dhall.ListLit (pure $ makeMapType c)
--     . HashMap.foldMapWithKey
--       ( \k ->
--           pure
--             . Dhall.RecordLit
--             . makeMapEntry (Dhall.TextLit $ Dhall.Chunks [] k)
--             . (`go` c)
--       )
--     $ coerce a

makeMapType :: Autodo.ValueCodec a void -> Dhall.Expr s a'
makeMapType = Dhall.Record . makeMapEntry Dhall.Text . toDhallTypeVia

makeMapEntry ::
  Dhall.Expr s a -> Dhall.Expr s a -> Map Text (Dhall.RecordField s a)
makeMapEntry k v =
  Map.fromList
    [ ("mapKey", Dhall.makeRecordField k),
      ("mapValue", Dhall.makeRecordField v)
    ]
