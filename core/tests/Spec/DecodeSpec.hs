{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Unsafe #-}

module Spec.DecodeSpec (spec) where

import "autodocodec" Autodocodec.Class qualified as Autodo
import "autodocodec" Autodocodec.Codec qualified as Autodo
import safe "autodocodec-dhall" Autodocodec.Dhall qualified as This
import safe "base" Control.Applicative (pure)
import safe "base" Control.Category ((.))
import safe "base" Control.Monad ((=<<))
import safe "base" Control.Monad.IO.Class (liftIO)
import safe "base" Data.Bool (Bool (True))
import safe "base" Data.Either (Either (Left))
import safe "base" Data.Eq (Eq, (==))
import safe "base" Data.Function (($))
import safe "base" Data.Int (Int, Int8)
import safe "base" Data.List.NonEmpty (NonEmpty ((:|)))
import safe "base" Data.Maybe (Maybe (Nothing))
import safe "base" Data.String (String)
import safe "base" Text.Show (Show)
import "dhall" Dhall qualified (input)
import "dhall" Dhall.Marshal.Decode qualified as Dhall (Decoder)
import "dhall" Dhall.Marshal.Decode qualified as Dhall.Decode
import "hspec" Test.Hspec
  ( Spec,
    SpecWith,
    anyException,
    describe,
    it,
    shouldBe,
    shouldThrow,
    xdescribe,
    xit,
  )
import "hspec-expectations" Test.Hspec.Expectations (Expectation)
import "string-interpolate" Data.String.Interpolate (__i)
import safe "text" Data.Text (Text)
import "this" Spec.Types (Ainur (Maiar), Fruit (Apple, Orange), War (OtherWar))
import safe "base" Prelude ((*))

shouldParseAs :: (Eq a, Show a) => (Dhall.Decoder a, Text) -> a -> Expectation
shouldParseAs (decoder, code) value =
  (`shouldBe` value) =<< liftIO (Dhall.input decoder code)

spec :: Spec
spec = do
  describe "decode" decode
  describe "decodeObject" decodeObject
  describe "decodeViaCodec" decodeViaCodec

decode :: SpecWith ()
decode = do
  describe "rmapCodec" do
    it "applies a function to the output side" do
      (This.decode $ Autodo.rmapCodec (* 2) Autodo.codec, "+5")
        `shouldParseAs` (10 :: Int)

  describe "eitherCodec" do
    it "parses disjoint types into `Either`" do
      let c =
            Autodo.eitherCodec Autodo.codec Autodo.codec ::
              Autodo.JSONCodec (Either Int String)
      ( This.decode c,
        [__i|< Left : Integer | Right : Text >.Right "world"|]
        )
        `shouldParseAs` pure "world"
    it "parses disjoint values without `Either`" do
      let c =
            Autodo.disjointEitherCodec
              (Autodo.codec :: Autodo.JSONCodec Int)
              (Autodo.codec :: Autodo.JSONCodec Int)
      (This.decode c, "+5") `shouldParseAs` Left 5
  describe "singleOrListCodec" do
    it "..." do
      let c = Autodo.singleOrListCodec Autodo.codec :: Autodo.JSONCodec [Int]
      (This.decode c, "< Left : Integer | Right : List Integer >.Left +5")
        `shouldParseAs` [5]
      ( This.decode c,
        "< Left : Integer | Right : List Integer >.Right [ +5, +6 ]"
        )
        `shouldParseAs` [5, 6]
  describe "singleOrNonEmptyCodec" do
    it "..." do
      let c =
            Autodo.singleOrNonEmptyCodec Autodo.codec ::
              Autodo.JSONCodec (NonEmpty Int)
      (This.decode c, "< Left : Integer | Right : List Integer >.Left +5")
        `shouldParseAs` (5 :| [])
      ( This.decode c,
        "< Left : Integer | Right : List Integer >.Right [ +5, +6 ]"
        )
        `shouldParseAs` (5 :| [6])
  describe "nullCodec" do
    it "..." do
      (This.decode Autodo.nullCodec, "{=}") `shouldParseAs` ()
      Dhall.input (This.decode Autodo.nullCodec) "+5" `shouldThrow` anyException
  describe "scientificCodec" do
    it "..." do
      (This.decode Autodo.scientificCodec, [__i|3.0|]) `shouldParseAs` 3.0
  describe "integerCodec" do
    it "..." do
      (This.decode Autodo.integerCodec, "-4") `shouldParseAs` -4
      ( This.decode Autodo.integerCodec,
        [__i|
          let sq = \\(x : Natural) -> x * x
           in Natural/toInteger (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq 99999)))))))))))
        |]
        )
        `shouldParseAs` ( let sq x = x * x
                           in sq . sq . sq . sq . sq . sq . sq . sq . sq . sq $
                                sq 99999
                        )
  describe "naturalCodec" do
    it "..." do
      (This.decode Autodo.naturalCodec, "4") `shouldParseAs` 4
      ( This.decode Autodo.naturalCodec,
        [__i|
          let sq = \\(x : Natural) -> x * x
           in sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq 99999))))))))))
        |]
        )
        `shouldParseAs` ( let sq x = x * x
                           in sq . sq . sq . sq . sq . sq . sq . sq . sq . sq $
                                sq 99999
                        )
  describe "scientificWithBoundsCodec" do
    it "..." do
      let c =
            Autodo.scientificWithBoundsCodec
              Autodo.Bounds {boundsLower = pure 2, boundsUpper = pure 4}
      (This.decode c, "-4.0") `shouldParseAs` -4
      ( This.decode c,
        [__i|
          let sq = \\(x : Natural) -> x * x
           in Integer/toDouble (Natural/toInteger (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq (sq 99999))))))))))))
        |]
        )
        `shouldParseAs` 1.797693134862316e308
  describe "integerWithBoundsCodec" do
    it "..." do
      let c =
            Autodo.integerWithBoundsCodec
              Autodo.Bounds {boundsLower = pure 2, boundsUpper = pure 4}
      (This.decode c, "3") `shouldParseAs` 3
      Dhall.input (This.decode c) "5" `shouldThrow` anyException
  describe "boundedIntegralCodec" do
    it "..." do
      let c = Autodo.boundedIntegralCodec :: Autodo.JSONCodec Int8
      (This.decode c, "+100") `shouldParseAs` 100
      Dhall.input (This.decode c) "+200" `shouldThrow` anyException
  describe "literalTextCodec" do
    it "..." do
      let c = Autodo.literalTextCodec "hello"
      (This.decode c, [__i|"hello"|]) `shouldParseAs` "hello"
      Dhall.input (This.decode c) [__i|"world"|] `shouldThrow` anyException
  describe "literalTextValueCodec" do
    it "..." do
      let c = Autodo.literalTextValueCodec True "yes"
      (This.decode c, [__i|"yes"|]) `shouldParseAs` True
      Dhall.input (This.decode c) [__i|"no"|] `shouldThrow` anyException
  describe "matchChoiceCodec" do
    it "..." do
      let c =
            Autodo.matchChoiceCodec
              (Autodo.literalTextCodec "even")
              (Autodo.literalTextCodec "odd")
              \s -> if s == "even" then Left s else pure s
      ( This.decode c,
        [__i|< Left : Text | Right : Text >.Left "even"|]
        )
        `shouldParseAs` "even"
      ( This.decode c,
        [__i|< Left : Text | Right : Text >.Right "odd"|]
        )
        `shouldParseAs` "odd"
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
      ( This.decode c,
        [__i|< Left : Text | Right : < Left : Text | Right : Text > >.Left "even"|]
        )
        `shouldParseAs` "even"
      ( This.decode c,
        [__i|< Left : Text | Right : < Left : Text | Right : Text > >.Right (< Left : Text | Right : Text >.Left "odd")|]
        )
        `shouldParseAs` "odd"
      Dhall.input (This.decode c) [__i|"foobar"|] `shouldThrow` anyException
      ( This.decode c,
        [__i|< Left : Text | Right : < Left : Text | Right : Text > >.Right (< Left : Text | Right : Text >.Right "fallback")|]
        )
        `shouldParseAs` "fallback"
  describe "parseAlternatives" do
    it "..." do
      let c =
            Autodo.parseAlternatives
              Autodo.shownBoundedEnumCodec
              [Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]]
      (This.decode c, [__i|< Left : Text | Right : Text >.Right "foo"|])
        `shouldParseAs` Apple
      (This.decode c, [__i|< Left : Text | Right : Text >.Left "Apple"|])
        `shouldParseAs` Apple
      Dhall.input (This.decode c) [__i|"Tomato"|]
        `shouldThrow` anyException
  describe "parseAlternative" do
    let c =
          Autodo.parseAlternative Autodo.shownBoundedEnumCodec $
            Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]
    it "..." do
      (This.decode c, [__i|< Left : Text | Right : Text >.Right "foo"|])
        `shouldParseAs` Apple
    it "..." do
      (This.decode c, [__i|< Left : Text | Right : Text >.Left "Apple"|])
        `shouldParseAs` Apple
  describe "stringConstCodec" do
    it "..." do
      let c = Autodo.stringConstCodec [(Apple, "foo"), (Orange, "bar")]
      (This.decode c, [__i|"foo"|]) `shouldParseAs` Apple
  describe "boundedEnumCodec" do
    it "..." do
      let c = Autodo.boundedEnumCodec $ \case
            Apple -> "foo"
            Orange -> "bar"
      (This.decode c, [__i|"bar"|]) `shouldParseAs` Orange
  describe "shownBoundedEnumCodec" do
    it "..." do
      let c = Autodo.shownBoundedEnumCodec
      (This.decode c, [__i|"Orange"|]) `shouldParseAs` Orange
  -- TODO: This relies on the unimplemented `ValueCodec`.
  xdescribe "codecViaAeson" do
    it "..." do
      (This.decode $ Autodo.codecViaAeson "Int", "+5") `shouldParseAs` (5 :: Int)

decodeObject :: SpecWith ()
decodeObject = do
  describe "parseAlternative" do
    let c = Autodo.shownBoundedEnumCodec
    it "..." do
      let o =
            Autodo.parseAlternative
              (Autodo.requiredFieldWith "current" c "current key for this field")
              (Autodo.requiredFieldWith "legacy" c "legacy key for this field")
      ( Dhall.Decode.record $ This.decodeObject o,
        [__i|{ undefined = < Left : { current : Text } | Right : { legacy : Text } >.Left { current = "Apple" } }|]
        )
        `shouldParseAs` Apple
      ( Dhall.Decode.record $ This.decodeObject o,
        [__i|{ undefined = < Left : { current : Text } | Right : { legacy : Text } >.Right { legacy = "Apple" } }|]
        )
        `shouldParseAs` Apple
      Dhall.input
        (Dhall.Decode.record $ This.decodeObject o :: Dhall.Decoder Fruit)
        [__i|{ undefined = < Left : { current : Text } | Right : { legacy : Text } >.Left { current = "Tomato" } }|]
        `shouldThrow` anyException
    let o =
          Autodo.parseAlternative
            (Autodo.optionalFieldWith "current" c "current key for this field")
            $ Autodo.optionalFieldWith "legacy" c "legacy key for this field"
    it "parses first optional field when possible" do
      ( Dhall.Decode.record $ This.decodeObject o,
        [__i|{ undefined = < Left : { current : Optional Text } | Right : { legacy : Optional Text } >.Left { current = Some "Apple" } }|]
        )
        `shouldParseAs` pure Apple
    xit "returns `Nothing` when missing optional field, rather than moving to alternative" do
      ( Dhall.Decode.record $ This.decodeObject o :: Dhall.Decoder (Maybe Fruit),
        [__i|{ undefined = < Left : { current : Optional Text } | Right : { legacy : Optional Text } >.Right { legacy = Some "Apple" } }|]
        )
        `shouldParseAs` Nothing
    xit "returns `Nothing` when first parser fails, and second is missing." do
      ( Dhall.Decode.record $ This.decodeObject o :: Dhall.Decoder (Maybe Fruit),
        [__i|{ undefined = < Left : { current : Optional Text } | Right : { legacy : Optional Text } >.Left { current = Some "Tomato" } }|]
        )
        `shouldParseAs` Nothing

decodeViaCodec :: SpecWith ()
decodeViaCodec = do
  describe "eitherCodec" do
    it "parses disjoint types into `Either`" do
      ( This.decodeViaCodec,
        [__i|< Left : Natural | Right : Text >.Right "of the roses"|]
        )
        `shouldParseAs` OtherWar "of the roses"
  describe "possiblyJointEitherCodec" do
    it "..." do
      ( This.decodeViaCodec,
        [__i|< Left : { domain : Text, name : Text } | Right : { name : Text } >.Right { name = "Olorin" }|]
        )
        `shouldParseAs` Maiar "Olorin"
