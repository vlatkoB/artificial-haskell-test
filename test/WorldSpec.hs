module WorldSpec (spec) where

import           ClassyPrelude
import qualified Data.ByteString.Char8 as BC (ByteString)
import           Test.Hspec

import           Template              (spiralWorldTemplate, zigZagWorldTemplate)
import           World                 (World (..), addCoords, findLargestIsland, mkWorld,
                                        mkWorldByTemplate, parseWorld,
                                        parsedWorldEqualString)

okScroll :: BC.ByteString
okScroll = "#~~~##~~#~~###~#~~##~#~#~"

parsedScroll :: String
parsedScroll = "~~~~3~~~2~~2~~~1~~2~~1~1~"

badScroll :: BC.ByteString
badScroll = "~~~##~~#~~###~#~~##~#~#~x"

chunkSize :: Int
chunkSize = 3000

spec :: Spec
spec = do
  parsedWorld <- runIO $ parseWorld chunkSize okScroll
  let spiralWorld  = mkWorldByTemplate spiralWorldTemplate parsedWorld
      spiralCoords = addCoords spiralWorld
      zigZagWorld  = mkWorldByTemplate zigZagWorldTemplate parsedWorld
      zigZagCoords = addCoords zigZagWorld

  describe "Parser can" $ do
    it "parse valid scroll" $
      parsedWorldEqualString parsedWorld parsedScroll `shouldBe` True

    it "reject invalid scroll" $ do
      badWorld <- parseWorld 10 badScroll
      parsedWorldEqualString badWorld "" `shouldBe` False
      -- evaluate (parseWorld 10 badScroll) `shouldThrow` anyErrorCall


  describe "Template can" $ do
    it "make Spiral World" $
      spiralWorld `shouldBe` World [ "~1~1~"
                                   , "~~~2~"
                                   , "2~~~~"
                                   , "~3~~2"
                                   , "~1~~~"
                                   ]

    it "set coords for Spiral World" $ spiralCoords `shouldBe` mapFromList [ ((1,3),'2')
                                                                           , ((2,1),'1')
                                                                           , ((2,4),'3')
                                                                           , ((2,5),'1')
                                                                           , ((4,1),'1')
                                                                           , ((4,2),'2')
                                                                           , ((5,4),'2')
                                                                           ]

    it "make ZigZag World" $ zigZagWorld `shouldBe` World [ "~~~~3"
                                                          , "~2~~~"
                                                          , "~2~~~"
                                                          , "~2~~1"
                                                          , "~1~1~"
                                                          ]

    it "set coords for ZigZag World" $ zigZagCoords `shouldBe` mapFromList [ ((2,2),'2')
                                                                           , ((2,3),'2')
                                                                           , ((2,4),'2')
                                                                           , ((2,5),'1')
                                                                           , ((4,5),'1')
                                                                           , ((5,1),'3')
                                                                           , ((5,4),'1')
                                                                           ]

  describe "Max population for" $ do
    let directSpiralCoords = mkWorld parsedWorld
    it "template Spiral World" $ findLargestIsland spiralCoords       `shouldBe` 6
    it "direct ZigZag World"   $ findLargestIsland zigZagCoords       `shouldBe` 7
    it "direct Spiral World"   $ findLargestIsland directSpiralCoords `shouldBe` 6
