module WorldSpec (spec) where

import ClassyPrelude
import Test.Hspec

import Template
import World

theScroll :: String
theScroll = "#~~~##~~#~~###~#~~##~#~#~"

spec :: Spec
spec = describe "World can" $ do
  it "parse population" $
    parseWorld theScroll `shouldBe`  ParsedWorld "~~~~3~~~2~~2~~~1~~2~~1~1~"
  it "make Spiral World" $ do
    let pw = parseWorld theScroll
    mkWorldByTemplate spiralWorldTemplate pw
      `shouldBe` [ "~1~1~"
                 , "~~~2~"
                 , "2~~~~"
                 , "~3~~2"
                 , "~1~~~"
                 ]
  it "set coords for villages" $ do
    let sw = mkWorldByTemplate spiralWorldTemplate $ parseWorld theScroll
    addCoords sw `shouldBe` mapFromList [ (1,[ ('1', (1, 2))
                                             , ('1', (1, 4))
                                             ]
                                          )
                                        , (2,[('2', (2, 4))])
                                        , (3,[('2', (3, 1))])
                                        , (4,[ ('3', (4, 2))
                                             , ('2', (4, 5))
                                             ]
                                          )
                                        , (5,[('1', (5, 2))])
                                        ]
  it "make spot villages" $ do
    let coords = addCoords . mkWorldByTemplate spiralWorldTemplate
                           $ parseWorld theScroll
    mkIslands coords `shouldBe` [ [('1',(1,2))]
                                , [('1',(1,4)), ('2',(2,4))]
                                , [('3',(4,2)), ('1',(5,2)), ('2',(3,1))]
                                , [('2',(4,5))]
                                ]

  it "sum village population" $ do
    let vlgs = mkIslands . addCoords
                         . mkWorldByTemplate spiralWorldTemplate
                         $ parseWorld theScroll
    calcIslandPopulation vlgs `shouldBe` [1,3,6,2]
