module PointCoordSpec ( spec ) where

import ClassyPrelude
import Data.List     (elemIndex)
import Test.Hspec    (Spec, describe, it, shouldBe)


import Template      (Coord, seqToCoordSpiral)

spec :: Spec
spec = do
  describe "Calculate coordinates by position" $ do
    it "in dimension  3" $ checkDimPositions  3 `shouldBe` True
    it "in dimension  4" $ checkDimPositions  4 `shouldBe` True
    it "in dimension  5" $ checkDimPositions  5 `shouldBe` True
    it "in dimension  6" $ checkDimPositions  6 `shouldBe` True
    it "in dimension  7" $ checkDimPositions  7 `shouldBe` True
    it "in dimension  8" $ checkDimPositions  8 `shouldBe` True
    it "in dimension  9" $ checkDimPositions  9 `shouldBe` True
    it "in dimension 10" $ checkDimPositions 10 `shouldBe` True

  where
    checkDimPositions dim =
      all (\i -> coordInt dim i == seqToCoordSpiral dim i) [1 .. dim * dim]

-- | Find coordinates of a number in matrix
coordInt :: Int -> Int -> Coord
coordInt dim n = case dim of
  3  -> go 1 m3
  4  -> go 1 m4
  5  -> go 1 m5
  6  -> go 1 m6
  7  -> go 1 m7
  8  -> go 1 m8
  9  -> go 1 m9
  10 -> go 1 m10
  _  -> go 0 []

  where
    go _ []       = (-1,-1)
    go x (xs:xss) = maybe (go (x+1) xss) ((,x) . (+1)) $ elemIndex n xs

------------------------------------------------------------------------------------------
-- Matrices
------------------------------------------------------------------------------------------
m3 :: [[Int]]
--       1  2  3
m3 = [ [ 7, 8, 9] -- 1  -- R DD LL
     , [ 6, 1, 2] -- 2
     , [ 5, 4, 3] -- 3
     ]

m4 :: [[Int]]
--       1  2  3  4
m4 = [ [ 7, 8, 9,10] -- 1  -- R DDD LLL
     , [ 6, 1, 2,11] -- 2
     , [ 5, 4, 3,12] -- 3
     , [16,15,14,13] -- 4
     ]

m5 :: [[Int]]
--       1  2  3  4  5
m5 = [ [21,22,23,24,25] -- 1  -- L UUUU RRRR
     , [20, 7, 8, 9,10] -- 2
     , [19, 6, 1, 2,11] -- 3
     , [18, 5, 4, 3,12] -- 4
     , [17,16,15,14,13] -- 5
     ]

m6 :: [[Int]]
--       1  2  3  4  5  6
m6 = [ [21,22,23,24,25,26] -- 1  -- R DDDDD LLLLL
     , [20, 7, 8, 9,10,27] -- 2
     , [19, 6, 1, 2,11,28] -- 3
     , [18, 5, 4, 3,12,29] -- 4
     , [17,16,15,14,13,30] -- 5
     , [36,35,34,33,32,31] -- 6
     ]

m7 :: [[Int]]
--       1  2  3  4  5  6  7
m7 = [ [43,44,45,46,47,48,49] -- 1   -- R DDDDDD LLLLLL
     , [42,21,22,23,24,25,26] -- 2
     , [41,20, 7, 8, 9,10,27] -- 3
     , [40,19, 6, 1, 2,11,28] -- 4
     , [39,18, 5, 4, 3,12,29] -- 5
     , [38,17,16,15,14,13,30] -- 6
     , [37,36,35,34,33,32,31] -- 7
     ]

m8 :: [[Int]]
--       1  2  3  4  5  6  7  8
m8 = [ [43,44,45,46,47,48,49,50] -- 1   -- R DDDDDDD LLLLLLL
     , [42,21,22,23,24,25,26,51] -- 2
     , [41,20, 7, 8, 9,10,27,52] -- 3
     , [40,19, 6, 1, 2,11,28,53] -- 4
     , [39,18, 5, 4, 3,12,29,54] -- 5
     , [38,17,16,15,14,13,30,55] -- 6
     , [37,36,35,34,33,32,31,56] -- 7
     , [64,63,62,61,60,59,58,57] -- 8
     ]

m9 :: [[Int]]
--       1  2  3  4  5  6  7  8  9
m9 = [ [73,74,75,76,77,78,79,80,81] -- 1 -- R DDDDDDDD LLLLLLLL
     , [72,43,44,45,46,47,48,49,50] -- 2
     , [71,42,21,22,23,24,25,26,51] -- 3
     , [70,41,20, 7, 8, 9,10,27,52] -- 4
     , [69,40,19, 6, 1, 2,11,28,53] -- 5
     , [68,39,18, 5, 4, 3,12,29,54] -- 6
     , [67,38,17,16,15,14,13,30,55] -- 7
     , [66,37,36,35,34,33,32,31,56] -- 8
     , [65,64,63,62,61,60,59,58,57] -- 9
     ]

m10 :: [[Int]]
--         1  2  3  4  5  6  7  8  9 10
m10 = [ [ 73,74,75,76,77,78,79,80,81,82] -- 1 -- R DDDDDDDD LLLLLLLL
      , [ 72,43,44,45,46,47,48,49,50,83] -- 2
      , [ 71,42,21,22,23,24,25,26,51,84] -- 3
      , [ 70,41,20, 7, 8, 9,10,27,52,85] -- 4
      , [ 69,40,19, 6, 1, 2,11,28,53,86] -- 5
      , [ 68,39,18, 5, 4, 3,12,29,54,87] -- 6
      , [ 67,38,17,16,15,14,13,30,55,88] -- 7
      , [ 66,37,36,35,34,33,32,31,56,89] -- 8
      , [ 65,64,63,62,61,60,59,58,57,90] -- 9
      , [100,99,98,97,96,95,94,93,92,91] -- 10
      ]
