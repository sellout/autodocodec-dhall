{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Unsafe #-}

module Spec.EncodeSpec (spec) where

import "autodocodec" Autodocodec.Class qualified as Autodo
import "autodocodec" Autodocodec.Codec qualified as Autodo
import safe "autodocodec-dhall" Autodocodec.Dhall qualified as This
import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Data.Bool (Bool (False, True))
import safe "base" Data.Char (Char)
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Eq ((==))
import safe "base" Data.Function (flip, ($))
import safe "base" Data.Int (Int, Int8)
import safe "base" Data.List.NonEmpty (NonEmpty ((:|)))
import safe "base" Data.Maybe (Maybe (Nothing))
import safe "base" Data.String (String)
import safe "base" Data.Tuple (uncurry)
import safe "base" Data.Void (Void)
import "dhall" Dhall.Core qualified as Dhall (Expr (..))
import "dhall" Dhall.Marshal.Encode qualified as Dhall
  ( Encoder (Encoder),
    declared,
    embed,
    recordEncoder,
  )
import "dhall" Dhall.Pretty qualified as Dhall (prettyExpr)
import "hspec" Test.Hspec (Spec, describe, it, shouldBe)
import "hspec-expectations" Test.Hspec.Expectations (Expectation)
import safe "prettyprinter" Prettyprinter qualified as Pretty
import safe "prettyprinter" Prettyprinter.Render.Text qualified as Pretty
  ( renderStrict,
  )
import "string-interpolate" Data.String.Interpolate (__i)
import safe "text" Data.Text (Text)
import "vector" Data.Vector qualified as Vector
import "this" Spec.Types
  ( Ainur (Maiar, Valar),
    Fruit (Apple, Orange),
    War (OtherWar, WorldWar),
  )
import safe "base" Prelude ((*), (-))

-- | The number of columns we want to maintain for the contents of this file.
codeWidth :: Int
codeWidth = 80

-- |
--
--  __NB__: This sets the Dhall printer to 70 columns (instead of 80) so that
--          the quasi-quoted output doesn’t overflow 80 columns with the
--          indentation.
prettyExpr ::
  -- | How far the quasi-quoted expression is indented in the source file. This
  --   is used to prevent the expressions from being too wide. It should
  --   generally be set via `let ?qqIndent = …` at the beginning of each `Spec`
  --   function.
  Int ->
  Dhall.Expr s Void ->
  Text
prettyExpr qqIndent =
  Pretty.renderStrict
    . Pretty.layoutSmart
      Pretty.LayoutOptions
        { layoutPageWidth = Pretty.AvailablePerLine (codeWidth - qqIndent) 1
        }
    . Dhall.prettyExpr

shouldShow :: (?qqIndent :: Int) => Dhall.Expr s Void -> Text -> Expectation
shouldShow = shouldBe . prettyExpr ?qqIndent

prettyEncoder :: Int -> Dhall.Encoder a -> a -> Text
prettyEncoder qqIndent Dhall.Encoder {embed, declared} =
  prettyExpr qqIndent . flip Dhall.Annot declared . embed

shouldShow' :: (?qqIndent :: Int) => (Dhall.Encoder a, a) -> Text -> Expectation
shouldShow' = shouldBe . uncurry (prettyEncoder ?qqIndent)

spec :: Spec
spec = do
  describe "encode" encode
  describe "encodeObject" encodeObject
  describe "encodeViaCodec" encodeViaCodec
  describe "embedVia" embedVia
  describe "embedViaCodec" embedViaCodec
  describe "declaredVia" declaredVia
  describe "declaredObjectVia" declaredObjectVia
  describe "embedObjectVia" embedObjectVia

encode :: Spec
encode = do
  let ?qqIndent = 10
  describe "lmapCodec" do
    it "applies a function to the input side" do
      (This.encode $ Autodo.lmapCodec (* 2) Autodo.codec, 5 :: Int)
        `shouldShow'` "+10 : Integer"
  describe "maybeCodec" do
    it "creates an either with an empty `Left`" do
      (This.encode $ Autodo.maybeCodec Autodo.codec, pure 'a')
        `shouldShow'` [__i|
          < Left | Right : Text >.Right "a" : < Left | Right : Text >
        |]
      (This.encode $ Autodo.maybeCodec Autodo.codec, Nothing :: Maybe Char)
        `shouldShow'` "< Left | Right : Text >.Left : < Left | Right : Text >"
  describe "eitherCodec" do
    it "..." do
      let c =
            Autodo.eitherCodec Autodo.codec Autodo.codec ::
              Autodo.JSONCodec (Either Int String)
      (This.encode c, Left 5)
        `shouldShow'` [__i|
            < Left : Integer | Right : Text >.Left +5
          : < Left : Integer | Right : Text >
        |]
      (This.encode c, pure "hello")
        `shouldShow'` [__i|
            < Left : Integer | Right : Text >.Right "hello"
          : < Left : Integer | Right : Text >
        |]
      (This.encode $ Autodo.maybeCodec Autodo.codec, Nothing :: Maybe Char)
        `shouldShow'` "< Left | Right : Text >.Left : < Left | Right : Text >"
  describe "disjointEitherCodec" do
    it "..." do
      let c =
            Autodo.disjointEitherCodec
              (Autodo.codec :: Autodo.JSONCodec Int)
              (Autodo.codec :: Autodo.JSONCodec Int)
      (This.encode c, Left 5) `shouldShow'` "+5 : Integer"
      (This.encode c, pure 6) `shouldShow'` "+6 : Integer"
  describe "vectorCodec" do
    it "..." do
      ( This.encode $ Autodo.vectorCodec Autodo.codec,
        Vector.fromList ['a', 'b']
        )
        `shouldShow'` [__i|[ "a", "b" ] : List Text|]
  describe "nonEmptyCodec" do
    it "..." do
      (This.encode $ Autodo.nonEmptyCodec Autodo.codec, 'a' :| ['b'])
        `shouldShow'` [__i|[ "a", "b" ] : List Text|]
  describe "singleOrListCodec" do
    it "..." do
      let c = Autodo.singleOrListCodec Autodo.codec
      (This.encode c, [5 :: Int])
        `shouldShow'` [__i|
            < Left : Integer | Right : List Integer >.Left +5
          : < Left : Integer | Right : List Integer >
        |]
      (This.encode c, [5, 6 :: Int])
        `shouldShow'` [__i|
            < Left : Integer | Right : List Integer >.Right [ +5, +6 ]
          : < Left : Integer | Right : List Integer >
        |]
  describe "singleOrNonEmptyCodec" do
    it "..." do
      let c = Autodo.singleOrNonEmptyCodec Autodo.codec
      (This.encode c, (5 :: Int) :| [])
        `shouldShow'` [__i|
            < Left : Integer | Right : List Integer >.Left +5
          : < Left : Integer | Right : List Integer >
        |]
      (This.encode c, 5 :| [6 :: Int])
        `shouldShow'` [__i|
            < Left : Integer | Right : List Integer >.Right [ +5, +6 ]
          : < Left : Integer | Right : List Integer >
        |]
  describe "nullCodec" do
    it "..." do
      (This.encode Autodo.nullCodec, ()) `shouldShow'` "{=} : {}"
  describe "boolCodec" do
    it "..." do
      (This.encode Autodo.boolCodec, True) `shouldShow'` "True : Bool"
  describe "textCodec" do
    it "..." do
      (This.encode Autodo.textCodec, "hello")
        `shouldShow'` [__i|"hello" : Text|]
  describe "stringCodec" do
    it "..." do
      (This.encode Autodo.stringCodec, "hello")
        `shouldShow'` [__i|"hello" : Text|]
      (This.encode Autodo.stringCodec, "\55296")
        `shouldShow'` "\"\65533\" : Text"
  describe "scientificCodec" do
    it "..." do
      (This.encode Autodo.scientificCodec, 5)
        `shouldShow'` [__i|5.0 : Double|]
  describe "integerCodec" do
    it "..." do
      (This.encode Autodo.integerCodec, 5) `shouldShow'` "+5 : Integer"
      (This.encode Autodo.integerCodec, -1_000_000_000_000)
        `shouldShow'` "-1000000000000 : Integer"
  describe "naturalCodec" do
    it "..." do
      (This.encode Autodo.naturalCodec, 5) `shouldShow'` "5 : Natural"
      (This.encode Autodo.naturalCodec, 1_000_000_000_000)
        `shouldShow'` "1000000000000 : Natural"
  describe "scientificWithBoundsCodec" do
    it "..." do
      let c = Autodo.scientificWithBoundsCodec . Autodo.Bounds (pure 2) $ pure 4
      (This.encode c, 3) `shouldShow'` "3.0 : Double"
      (This.encode c, 5) `shouldShow'` "5.0 : Double"
  describe "boundedIntegralCodec" do
    it "..." do
      (This.encode Autodo.boundedIntegralCodec, 5 :: Int8)
        `shouldShow'` "+5 : Integer"
  describe "literalTextCodec" do
    it "..." do
      let c = Autodo.literalTextCodec "hello"
      (This.encode c, "hello") `shouldShow'` [__i|"hello" : Text|]
      (This.encode c, "world") `shouldShow'` [__i|"hello" : Text|]
  describe "literalTextValueCodec" do
    it "..." do
      let c = Autodo.literalTextValueCodec True "yes"
      (This.encode c, True) `shouldShow'` [__i|"yes" : Text|]
      (This.encode c, False) `shouldShow'` [__i|"yes" : Text|]
  describe "matchChoiceCodec" do
    it "..." do
      let c =
            Autodo.matchChoiceCodec
              (Autodo.literalTextCodec "even")
              (Autodo.literalTextCodec "odd")
              \s -> if s == "even" then Left s else pure s
      (This.encode c, "even")
        `shouldShow'` [__i|
            < Left : Text | Right : Text >.Left "even"
          : < Left : Text | Right : Text >
        |]
      (This.encode c, "not even")
        `shouldShow'` [__i|
            < Left : Text | Right : Text >.Right "odd"
          : < Left : Text | Right : Text >
        |]
  describe "matchChoicesCodec" do
    it "..." do
      let c =
            Autodo.matchChoicesCodec
              [ ( \s -> if s == "even" then pure s else Nothing,
                  Autodo.literalTextCodec "even"
                ),
                ( \s -> if s == "odd" then pure s else Nothing,
                  Autodo.literalTextCodec "odd"
                )
              ]
              $ Autodo.literalTextCodec "fallback"
      (This.encode c, "even")
        `shouldShow'` [__i|
            < Left : Text | Right : < Left : Text | Right : Text > >.Left "even"
          : < Left : Text | Right : < Left : Text | Right : Text > >
        |]
      (This.encode c, "odd")
        `shouldShow'` [__i|
            < Left : Text | Right : < Left : Text | Right : Text > >.Right
              (< Left : Text | Right : Text >.Left "odd")
          : < Left : Text | Right : < Left : Text | Right : Text > >
        |]
      (This.encode c, "foobar")
        `shouldShow'` [__i|
            < Left : Text | Right : < Left : Text | Right : Text > >.Right
              (< Left : Text | Right : Text >.Right "fallback")
          : < Left : Text | Right : < Left : Text | Right : Text > >
        |]
  describe "parseAlternatives" do
    it "..." do
      let c =
            Autodo.parseAlternatives
              Autodo.shownBoundedEnumCodec
              [Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]]
      (This.encode c, Apple)
        `shouldShow'` [__i|
            < Left : Text | Right : Text >.Left "Apple"
          : < Left : Text | Right : Text >
        |]
  describe "parseAlternative" do
    it "..." do
      let c =
            Autodo.parseAlternative Autodo.shownBoundedEnumCodec $
              Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]
      (This.encode c, Apple)
        `shouldShow'` [__i|
            < Left : Text | Right : Text >.Left "Apple"
          : < Left : Text | Right : Text >
        |]
  describe "stringConstCodec" do
    it "..." do
      let c = Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]
      (This.encode c, Orange) `shouldShow'` [__i|"bar" : Text|]
    it "..." do
      let c = Autodo.stringConstCodec [(Apple, "foo")]
      (This.encode c, Orange) `shouldShow'` [__i|"foo" : Text|]

encodeObject :: Spec
encodeObject = do
  let ?qqIndent = 10
  describe "shownBoundedEnumCodec" do
    it "..." do
      let c = Autodo.shownBoundedEnumCodec
      let o =
            Autodo.parseAlternative
              ( Autodo.requiredFieldWith
                  "current"
                  c
                  "current key for this field"
              )
              $ Autodo.requiredFieldWith "legacy" c "legacy key for this field"
      (Dhall.recordEncoder $ This.encodeObject o, Apple)
        `shouldShow'` [__i|
            { undefined =
                < Left : { current : Text } | Right : { legacy : Text } >.Left
                  { current = "Apple" }
            }
          : { undefined :
                < Left : { current : Text } | Right : { legacy : Text } >
            }
        |]

encodeViaCodec :: Spec
encodeViaCodec = do
  let ?qqIndent = 8
  it "..." do
    (This.encodeViaCodec, WorldWar 2)
      `shouldShow'` [__i|
          < Left : Natural | Right : Text >.Left 2
        : < Left : Natural | Right : Text >
      |]
    (This.encodeViaCodec, OtherWar "OnDrugs")
      `shouldShow'` [__i|
          < Left : Natural | Right : Text >.Right "OnDrugs"
        : < Left : Natural | Right : Text >
      |]
    (This.encodeViaCodec, Valar "Stars" "Varda")
      `shouldShow'` [__i|
          < Left : { domain : Text, name : Text }
          | Right : { name : Text }
          >.Left
            { domain = "Stars", name = "Varda" }
        : < Left : { domain : Text, name : Text } | Right : { name : Text } >
      |]
    (This.encodeViaCodec, Maiar "Sauron")
      `shouldShow'` [__i|
          < Left : { domain : Text, name : Text }
          | Right : { name : Text }
          >.Right
            { name = "Sauron" }
        : < Left : { domain : Text, name : Text } | Right : { name : Text } >
      |]

embedVia :: Spec
embedVia = do
  let ?qqIndent = 10
  describe "lmapCodec" do
    it "..." do
      This.embedVia
        (Autodo.lmapCodec (* 2) (Autodo.codec :: Autodo.JSONCodec Int))
        5
        `shouldShow` "+10"
  describe "maybeCodec" do
    it "..." do
      This.embedVia (Autodo.maybeCodec Autodo.codec) (pure 'a')
        `shouldShow` [__i|< Left | Right : Text >.Right "a"|]
      This.embedVia (Autodo.maybeCodec Autodo.codec) (Nothing :: Maybe Char)
        `shouldShow` "< Left | Right : Text >.Left"
  describe "eitherCodec" do
    it "..." do
      let c =
            Autodo.eitherCodec Autodo.codec Autodo.codec ::
              Autodo.JSONCodec (Either Int String)
      This.embedVia c (Left 5)
        `shouldShow` "< Left : Integer | Right : Text >.Left +5"
      This.embedVia c (pure "hello")
        `shouldShow` [__i|< Left : Integer | Right : Text >.Right "hello"|]
  describe "disjointEitherCodec" do
    it "..." do
      let c =
            Autodo.disjointEitherCodec
              (Autodo.codec :: Autodo.JSONCodec Int)
              (Autodo.codec :: Autodo.JSONCodec Int)
      This.embedVia c (Left 5) `shouldShow` "+5"
      This.embedVia c (pure 6) `shouldShow` "+6"
  describe "integerCodec" do
    it "embeds integers as Integer" do
      This.embedVia Autodo.integerCodec 5 `shouldShow` "+5"
      This.embedVia Autodo.integerCodec -1_000_000_000_000
        `shouldShow` "-1000000000000"
  describe "naturalCodec" do
    it "embeds naturals as Natural" do
      This.embedVia Autodo.naturalCodec 5 `shouldShow` "5"
      This.embedVia Autodo.naturalCodec 1_000_000_000_000
        `shouldShow` "1000000000000"
  describe "scientificWithBoundsCodec" do
    it "embeds scientific as Double" do
      let c = Autodo.scientificWithBoundsCodec . Autodo.Bounds (pure 2) $ pure 4
      This.embedVia c 3 `shouldShow` "3.0"
      This.embedVia c 5 `shouldShow` "5.0"
  describe "boundedIntegralCodec" do
    it "embeds signed integrals as Integer" do
      let c = Autodo.boundedIntegralCodec :: Autodo.JSONCodec Int8
      This.embedVia c 5 `shouldShow` "+5"
  describe "literalTextCodec" do
    it "embeds literal text as Text" do
      let c = Autodo.literalTextCodec "hello"
      This.embedVia c "hello" `shouldShow` [__i|"hello"|]
      This.embedVia c "world" `shouldShow` [__i|"hello"|]
  describe "literalTextValueCodec" do
    it "..." do
      let c = Autodo.literalTextValueCodec True "yes"
      This.embedVia c True `shouldShow` [__i|"yes"|]
      This.embedVia c False `shouldShow` [__i|"yes"|]
  describe "matchChoiceCodec" do
    it "..." do
      let c =
            Autodo.matchChoiceCodec
              (Autodo.literalTextCodec "even")
              (Autodo.literalTextCodec "odd")
              \s -> if s == "even" then Left s else pure s
      This.embedVia c "even"
        `shouldShow` [__i|< Left : Text | Right : Text >.Left "even"|]
      This.embedVia c "not even"
        `shouldShow` [__i|< Left : Text | Right : Text >.Right "odd"|]
  describe "matchChoicesCodec" do
    it "..." do
      let c =
            Autodo.matchChoicesCodec
              [ ( \s -> if s == "even" then pure s else Nothing,
                  Autodo.literalTextCodec "even"
                ),
                ( \s -> if s == "odd" then pure s else Nothing,
                  Autodo.literalTextCodec "odd"
                )
              ]
              $ Autodo.literalTextCodec "fallback"
      This.embedVia c "even"
        `shouldShow` [__i|
        < Left : Text | Right : < Left : Text | Right : Text > >.Left "even"
      |]
      This.embedVia c "odd"
        `shouldShow` [__i|
        < Left : Text | Right : < Left : Text | Right : Text > >.Right
          (< Left : Text | Right : Text >.Left "odd")
      |]
      This.embedVia c "foobar"
        `shouldShow` [__i|
        < Left : Text | Right : < Left : Text | Right : Text > >.Right
          (< Left : Text | Right : Text >.Right "fallback")
      |]
  describe "parseAlternatives" do
    it "..." do
      let c =
            Autodo.parseAlternatives
              Autodo.shownBoundedEnumCodec
              [Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]]
      This.embedVia c Apple
        `shouldShow` [__i|< Left : Text | Right : Text >.Left "Apple"|]
  describe "parseAlternative" do
    it "..." do
      let c =
            Autodo.parseAlternative Autodo.shownBoundedEnumCodec $
              Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]
      This.embedVia c Apple
        `shouldShow` [__i|< Left : Text | Right : Text >.Left "Apple"|]
  describe "stringConstCodec" do
    it "..." do
      let c = Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]
      This.embedVia c Orange `shouldShow` [__i|"bar"|]
    it "..." do
      let c = Autodo.stringConstCodec [(Apple, "foo")]
      This.embedVia c Orange `shouldShow` [__i|"foo"|]

embedViaCodec :: Spec
embedViaCodec = do
  let ?qqIndent = 8
  it "..." do
    This.embedViaCodec (WorldWar 2)
      `shouldShow` [__i|< Left : Natural | Right : Text >.Left 2|]
    This.embedViaCodec (OtherWar "OnDrugs")
      `shouldShow` [__i|< Left : Natural | Right : Text >.Right "OnDrugs"|]
    This.embedViaCodec (Valar "Stars" "Varda")
      `shouldShow` [__i|
        < Left : { domain : Text, name : Text } | Right : { name : Text } >.Left
          { domain = "Stars", name = "Varda" }
      |]
    This.embedViaCodec (Maiar "Sauron")
      `shouldShow` [__i|
        < Left : { domain : Text, name : Text }
        | Right : { name : Text }
        >.Right
          { name = "Sauron" }
      |]
    This.embedVia (Autodo.vectorCodec Autodo.codec) (Vector.fromList ['a', 'b'])
      `shouldShow` [__i|[ "a", "b" ]|]

declaredVia :: Spec
declaredVia = do
  let ?qqIndent = 10
  describe "integerCodec" do
    it "..." do
      This.declaredVia Autodo.integerCodec `shouldShow` "Integer"
  describe "naturalCodec" do
    it "..." do
      This.declaredVia Autodo.naturalCodec `shouldShow` "Natural"
  describe "scientificWithBoundsCodec" do
    it "..." do
      let c =
            Autodo.scientificWithBoundsCodec
              Autodo.Bounds {boundsLower = pure 2, boundsUpper = pure 4}
      This.declaredVia c `shouldShow` "Double"
  describe "boundedIntegralCodec" do
    it "..." do
      This.declaredVia (Autodo.boundedIntegralCodec :: Autodo.JSONCodec Int8)
        `shouldShow` "Integer"
  describe "literalTextCodec" do
    it "..." do
      This.declaredVia (Autodo.literalTextCodec "hello") `shouldShow` "Text"
  describe "literalTextValueCodec" do
    it "..." do
      This.declaredVia (Autodo.literalTextValueCodec True "yes") `shouldShow` "Text"
  describe "matchChoiceCodec" do
    it "..." do
      This.declaredVia
        ( Autodo.matchChoiceCodec
            (Autodo.literalTextCodec "even")
            (Autodo.literalTextCodec "odd")
            \s -> if s == "even" then Left s else pure s
        )
        `shouldShow` "< Left : Text | Right : Text >"
  describe "matchChoicesCodec" do
    it "..." do
      This.declaredVia
        ( Autodo.matchChoicesCodec
            [ ( \s -> if s == "even" then pure s else Nothing,
                Autodo.literalTextCodec "even"
              ),
              ( \s -> if s == "odd" then pure s else Nothing,
                Autodo.literalTextCodec "odd"
              )
            ]
            $ Autodo.literalTextCodec "fallback"
        )
        `shouldShow` "< Left : Text | Right : < Left : Text | Right : Text > >"
  describe "parseAlternatives" do
    it "..." do
      This.declaredVia
        ( Autodo.parseAlternatives
            Autodo.shownBoundedEnumCodec
            [Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]]
        )
        `shouldShow` [__i|< Left : Text | Right : Text >|]
  describe "parseAlternative" do
    it "..." do
      This.declaredVia
        ( Autodo.parseAlternative Autodo.shownBoundedEnumCodec $
            Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]
        )
        `shouldShow` [__i|< Left : Text | Right : Text >|]
  describe "stringConstCodec" do
    it "..." do
      This.declaredVia (Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")])
        `shouldShow` [__i|Text|]
      This.declaredVia (Autodo.stringConstCodec [(Apple, "foo")])
        `shouldShow` "Text"

declaredObjectVia :: Spec
declaredObjectVia = do
  let ?qqIndent = 10
  describe "parseAlternative" do
    it "..." do
      let c = Autodo.shownBoundedEnumCodec
      let o =
            Autodo.parseAlternative
              (Autodo.requiredFieldWith "current" c "current key for this field")
              $ Autodo.requiredFieldWith "legacy" c "legacy key for this field" ::
              Autodo.ObjectCodec Fruit Fruit
      This.declaredObjectVia o
        `shouldShow` [__i|
        { undefined :
            < Left : { current : Text } | Right : { legacy : Text } >
        }
      |]

embedObjectVia :: Spec
embedObjectVia = do
  let ?qqIndent = 10
  describe "parseAlternative" do
    it "..." do
      let c = Autodo.shownBoundedEnumCodec
      let o =
            Autodo.parseAlternative
              (Autodo.requiredFieldWith "current" c "current key for this field")
              $ Autodo.requiredFieldWith "legacy" c "legacy key for this field"
      This.embedObjectVia o Apple
        `shouldShow` [__i|
        { undefined =
            < Left : { current : Text } | Right : { legacy : Text } >.Left
              { current = "Apple" }
        }
      |]
