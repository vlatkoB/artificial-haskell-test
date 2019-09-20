module Template
  ( Template (..)
  , Coord

  , mapOver
  , pointCoordInSpiral
  , spiralWorldTemplate
  , zigZagWorldTemplate
  )
where

import           ClassyPrelude

import qualified Data.ByteString.Char8 as BC
import           Data.List             (transpose)
import           GHC.Generics          (Generic)


------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------
-- | Shape how templates are represented
newtype Template = Template {unTemplate :: [[Int]]}
  deriving (Show, Eq, Generic)


------------------------------------------------------------------------------------------
-- Aliases
------------------------------------------------------------------------------------------
-- | Coordinates in 2d, both start with 1
type Coord  = (Int, Int)


------------------------------------------------------------------------------------------
-- Funcs
------------------------------------------------------------------------------------------
-- | Roll string of chars over the template
mapOver :: BC.ByteString -> Template -> [BC.ByteString]
mapOver s = map (BC.pack . map go) . unTemplate
  where go n = maybe ' ' fst . BC.uncons $ BC.drop (pred n) s


-- | Get the coordinates for a sequential point in an n-dimensional 2D square spiral.
--   Coord system is top-left based starting with (1,1)
pointCoordInSpiral :: Int -> Int -> Coord
pointCoordInSpiral 1 1 = (1,1)
pointCoordInSpiral 2 p =
  case p of
    1 -> (1,1)
    2 -> (2,1)
    3 -> (2,2)
    _ -> (1,2)
pointCoordInSpiral dimension position = do
  let fullDim = floor   @Float . sqrt $ fromIntegral position
      jumpDim = ceiling @Float $ fromIntegral (dimension - fullDim) / 2
      initPos = if even fullDim
                  then (1 + jumpDim, fullDim + jumpDim)
                  else (fullDim + jumpDim, 1 + jumpDim)
      newPos  = position - pow2 fullDim

  if position >= pow2 dimension
    then error "Matrix dimension too small for searched point"
    else go (fullDim + 1) newPos initPos
  where
    go !dim n (!x, !y) =
      if | n <= 0    -> (x, y)
         | even dim  -> (x + 1 - xMove, y + yMove)
         | odd  dim  -> (x - 1 + xMove, y - yMove)
         | otherwise -> error "should never be reached"

      where
        yMove = min (dim - 1) (n - 1)
        xMove = min (dim - 1) (n - 1 - yMove)

-- | Create a square spiral template starting in the middle and going clock-wise
spiralWorldTemplate :: Int -> Template
spiralWorldTemplate 1 = Template $ singleton (singleton 1)
spiralWorldTemplate n = Template $
  if odd n
    then oddRow <> zipWith (<>) (transpose $ map reverse cols) prevDimension
    else           zipWith (<>) prevDimension (transpose cols <> evenRow)    <> evenRow

  where
    evenRow       = [reverse row]
    oddRow        = [row]
    prevDimension = unTemplate $ spiralWorldTemplate $ pred n
    newElems      = [succ (pow2 (pred n)) .. pow2 n]
    (cols,row)    = first singleton $ splitAt (length newElems `div` 2) newElems

-- | Create a zig-zag template starting left-top and going down, like snake
zigZagWorldTemplate :: Int -> Template
zigZagWorldTemplate n = Template $ map mkList [0..n - 1]
  where mkList i = (if odd i then reverse else id) [(i * n + 1).. (i + 1) * n]


------------------------------------------------------------------------------------------
-- Helpers
------------------------------------------------------------------------------------------
pow2 :: Int -> Int
pow2 = (^ (2 :: Int))



------------------------------------------------------------------------------------------
-- Testing
------------------------------------------------------------------------------------------

-- m2 :: [[Int]]
-- m2 = [ [1,2]   -- RDL
--      , [4,3]
--      ]

-- m3 :: [[Int]]
-- m3 = [ [ 7, 8, 9]  -- LUURR
--      , [ 6, 1, 2]
--      , [ 5, 4, 3]
--      ]

-- m4 :: [[Int]]
-- m4 = [ [ 7, 8, 9,10]  -- RDDDLLL
--      , [ 6, 1, 2,11]
--      , [ 5, 4, 3,12]
--      , [16,15,14,13]
--      ]

-- m5 :: [[Int]]
-- m5 = [ [21,22,23,24,25]  -- LUUUURRRR
--      , [20, 7, 8, 9,10]
--      , [19, 6, 1, 2,11]
--      , [18, 5, 4, 3,12]
--      , [17,16,15,14,13]
--      ]

-- m6 :: [[Int]]
-- m6 = [ [21,22,23,24,25,26]  -- R DDDDD LLLLL
--      , [20, 7, 8, 9,10,27]
--      , [19, 6, 1, 2,11,28]
--      , [18, 5, 4, 3,12,29]
--      , [17,16,15,14,13,30]
--      , [36,35,34,33,32,31]
--      ]

-- m7 :: [[Int]]
-- m7 = [ [43,44,45,46,47,48,49]   -- R DDDDDD LLLLLL
--      , [42,21,22,23,24,25,26]
--      , [41,20, 7, 8, 9,10,27]
--      , [40,19, 6, 1, 2,11,28]
--      , [39,18, 5, 4, 3,12,29]
--      , [38,17,16,15,14,13,30]
--      , [37,36,35,34,33,32,31]
--      ]

-- m8 :: [[Int]]
-- m8 = [ [43,44,45,46,47,48,49,50]   -- R DDDDDDD LLLLLLL
--      , [42,21,22,23,24,25,26,51]
--      , [41,20, 7, 8, 9,10,27,52]
--      , [40,19, 6, 1, 2,11,28,53]
--      , [39,18, 5, 4, 3,12,29,54]
--      , [38,17,16,15,14,13,30,55]
--      , [37,36,35,34,33,32,31,56]
--      , [64,63,62,61,60,59,58,57]
--      ]

-- m9 :: [[Int]]
-- m9 = [ [73,74,75,76,77,78,79,80,81]  -- R DDDDDDDD LLLLLLLL
--      , [72,43,44,45,46,47,48,49,50]
--      , [71,42,21,22,23,24,25,26,51]
--      , [70,41,20, 7, 8, 9,10,27,52]
--      , [69,40,19, 6, 1, 2,11,28,53]
--      , [68,39,18, 5, 4, 3,12,29,54]
--      , [67,38,17,16,15,14,13,30,55]
--      , [66,37,36,35,34,33,32,31,56]
--      , [65,64,63,62,61,60,59,58,57]
--      ]
