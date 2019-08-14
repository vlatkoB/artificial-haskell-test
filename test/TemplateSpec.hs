module TemplateSpec (spec) where

import ClassyPrelude
import Test.Hspec

import Template      (spiralWorldTemplate, zigZagWorldTemplate)

spec :: Spec
spec = do
  describe "SpiralWorld Template" $ do
    it "can create 2 dimensions" $ spiralWorldTemplate 2 `shouldBe` spiral2
    it "can create 4 dimensions" $ spiralWorldTemplate 4 `shouldBe` spiral4

  describe "ZigZagWorld Template" $ do
    it "can create 2 dimensions" $ zigZagWorldTemplate 2 `shouldBe` zigZag2
    it "can create 4 dimensions" $ zigZagWorldTemplate 4 `shouldBe` zigZag4


spiral2 :: [[Int]]
spiral2 = [ [1,2]
          , [4,3]
          ]

spiral4 :: [[Int]]
spiral4 = [ [ 7, 8, 9,10]
        , [ 6, 1, 2,11]
        , [ 5, 4, 3,12]
        , [16,15,14,13]
        ]


zigZag2 :: [[Int]]
zigZag2 = [ [1,2]
          , [4,3]
          ]

zigZag4 :: [[Int]]
zigZag4 = [ [ 1, 2, 3, 4]
          , [ 8, 7, 6, 5]
          , [ 9,10,11,12]
          , [16,15,14,13]
          ]
