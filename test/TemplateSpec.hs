module TemplateSpec (spec) where

import ClassyPrelude
import Test.Hspec    (describe, it, shouldBe, Spec)

import Template      (Template (..), pointCoordInSpiral, spiralWorldTemplate,
                      zigZagWorldTemplate)

spec :: Spec
spec = do
  describe "SpiralWorld Template can" $ do
    it "create 2 dimensions" $ spiralWorldTemplate 2 `shouldBe` spiral2
    it "create 4 dimensions" $ spiralWorldTemplate 4 `shouldBe` spiral4

  describe "ZigZagWorld Template can" $ do
    it "create 2 dimensions" $ zigZagWorldTemplate 2 `shouldBe` zigZag2
    it "create 4 dimensions" $ zigZagWorldTemplate 4 `shouldBe` zigZag4

  describe "Direct coordinates for SpiralWorld in" $ do
    it "2 dimensions" $ pointCoordInSpiral 2 2 `shouldBe` (2,1)
    it "5 dimensions" $ pointCoordInSpiral 5 2 `shouldBe` (4,3)
    it "9 dimensions" $ pointCoordInSpiral 9 2 `shouldBe` (6,5)



spiral2, spiral4, zigZag2, zigZag4 :: Template
spiral2 = Template [ [1,2]
                   , [4,3]
                   ]
spiral4 = Template [ [ 7, 8, 9,10]
                   , [ 6, 1, 2,11]
                   , [ 5, 4, 3,12]
                   , [16,15,14,13]
                   ]
zigZag2 = Template [ [1,2]
                   , [4,3]
                   ]
zigZag4 = Template [ [ 1, 2, 3, 4]
                   , [ 8, 7, 6, 5]
                   , [ 9,10,11,12]
                   , [16,15,14,13]
                   ]
