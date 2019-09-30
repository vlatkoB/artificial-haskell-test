module WorldSpec (spec) where

import           ClassyPrelude

import qualified Data.ByteString.Char8 as BC (ByteString)
import           Test.Hspec

import           Template              (spiralWorldTemplate, zigZagWorldTemplate)
import qualified Vector                as VecWorld
import           World                 (ProcessingKind (..), World (..), addCoords,
                                        findLargestIsland, mkWorld, mkWorldByTemplate,
                                        parseWorld, unParsedWorld)


okScroll :: BC.ByteString
okScroll = "#~~~##~~#~~###~#~~##~#~#~"

parsedScroll :: [Word8]
parsedScroll = [0,0,0,0,3,0,0,0,2,0,0,2,0,0,0,1,0,0,2,0,0,1,0,1,0]

badScroll :: BC.ByteString
badScroll = "~~~##~~#~~###~#~~##~#~#~x"

spec :: Spec
spec = do
  let parsedWorld        = parseWorld okScroll
      size               = length okScroll
      spiralWorld        = mkWorldByTemplate size spiralWorldTemplate parsedWorld
      spiralCoords       = addCoords spiralWorld
      zigZagWorld        = mkWorldByTemplate size zigZagWorldTemplate parsedWorld
      zigZagCoords       = addCoords zigZagWorld
      directSpiralCoords = mkWorld (length $ unParsedWorld parsedWorld)
                                    Sequential
                                    parsedWorld

  describe "Parser can" $ do
    it "parse valid scroll" $
      unParsedWorld parsedWorld `shouldBe` reverse parsedScroll

    it "reject invalid scroll" $ do
      evaluate (parseWorld badScroll) `shouldThrow` anyErrorCall

  describe "Parsers produce the same result" $ do
    it "Vectror and no template Spiral" $ do
      vecMax <- VecWorld.parseAndProcess size okScroll
      findLargestIsland directSpiralCoords `shouldBe` vecMax


  describe "Template can" $ do
    it "make Spiral World" $
      spiralWorld `shouldBe` World [ [0,1,0,1,0]
                                   , [0,0,0,2,0]
                                   , [2,0,0,0,0]
                                   , [0,3,0,0,2]
                                   , [0,1,0,0,0]
                                   ]

    it "set coords for Spiral World" $ spiralCoords `shouldBe` mapFromList [ ((1,3),2)
                                                                           , ((2,1),1)
                                                                           , ((2,4),3)
                                                                           , ((2,5),1)
                                                                           , ((4,1),1)
                                                                           , ((4,2),2)
                                                                           , ((5,4),2)
                                                                           ]

    it "make ZigZag World" $ zigZagWorld `shouldBe` World [ [0,0,0,0,3]
                                                          , [0,2,0,0,0]
                                                          , [0,2,0,0,0]
                                                          , [0,2,0,0,1]
                                                          , [0,1,0,1,0]
                                                          ]

    it "set coords for ZigZag World" $ zigZagCoords `shouldBe` mapFromList [ ((2,2),2)
                                                                           , ((2,3),2)
                                                                           , ((2,4),2)
                                                                           , ((2,5),1)
                                                                           , ((4,5),1)
                                                                           , ((5,1),3)
                                                                           , ((5,4),1)
                                                                           ]

  describe "Max population for" $ do
    it "template Spiral World" $ findLargestIsland spiralCoords       `shouldBe` 6
    it "direct ZigZag World"   $ findLargestIsland zigZagCoords       `shouldBe` 7
    it "direct Spiral World"   $ findLargestIsland directSpiralCoords `shouldBe` 6
