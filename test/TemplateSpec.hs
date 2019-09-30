module TemplateSpec (spec) where

import ClassyPrelude
import Test.Hspec      (Spec, describe, it, shouldBe)
import Test.QuickCheck (Arbitrary (..), choose, property)

import Template        (Template (..), coordToSeqSpiral, seqToCoordSpiral,
                        spiralWorldTemplate, zigZagWorldTemplate)

data DimSeq = DimSeq Int Int deriving (Show, Generic)
instance Arbitrary DimSeq where
  arbitrary = do
    dim <- choose (0,1000)
    DimSeq dim <$> choose (1,dim*dim)

spec :: Spec
spec = do
  describe "SpiralWorld Template can" $ do
    it "create 2 dimensions" $ spiralWorldTemplate 2 `shouldBe` spiral2
    it "create 4 dimensions" $ spiralWorldTemplate 4 `shouldBe` spiral4

  describe "ZigZagWorld Template can" $ do
    it "create 2 dimensions" $ zigZagWorldTemplate 2 `shouldBe` zigZag2
    it "create 4 dimensions" $ zigZagWorldTemplate 4 `shouldBe` zigZag4

  describe "Direct coordinates for SpiralWorld in" $ do
    it "2 dimensions" $ seqToCoordSpiral 2 2 `shouldBe` (2,1)
    it "5 dimensions" $ seqToCoordSpiral 5 2 `shouldBe` (4,3)
    it "9 dimensions" $ seqToCoordSpiral 9 2 `shouldBe` (6,5)

  describe "Convert between sequential position and" $ do
    it " coordinates in Spiral world" $ property $
      \(DimSeq dim pos ) -> do
        let coord = seqToCoordSpiral dim pos
            pos'  = coordToSeqSpiral dim coord
        pos == pos'



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
