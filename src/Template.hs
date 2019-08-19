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
pointCoordInSpiral dimension srch =
  if srch > pow2 dimension
    then error "Matrix dimension too small for searched point"
    else go 1 srch (middlePoint, middlePoint)
  where
    middlePoint     = ceiling @Float $ fromIntegral dimension / 2
    go !dim !n (!x, !y) = do
      if | n <= 0           -> (x, y)
         | dim == 1         -> nextDim subN (x,y)
         | n >= dimDiffArea -> nextDim (subN - (dim - 1) * 2) fullDim
         | even dim         -> nextDim partialN (x + 1 - xMove, y + yMove)
         | odd  dim         -> nextDim partialN (x - 1 + xMove, y - yMove)
         | otherwise        -> (x, y) -- should never be reached

      where
        maxMoves         = dim - 1
        subN             = n - 1
        yMove            = min maxMoves subN
        xMove            = min maxMoves (subN - yMove)
        partialN         = subN - xMove - yMove
        dimDiffArea      = pow2 dim - pow2 (dim-1)
        nextDim !n' !pos = go (dim + 1) n' pos
        fullDim          = if even dim
                             then (x + 1 - maxMoves, y + maxMoves)
                             else (x - 1 + maxMoves, y - maxMoves)

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

-- m9 :: [[Int]]
-- m9 = [ [73,74,75,76,77,78,79,81,82]  -- R DDDDDDDD LLLLLLLL
--      , [72,43,44,45,46,47,48,49,50]
--      , [71,42,21,22,23,24,25,26,51]
--      , [70,41,20, 7, 8, 9,10,27,52]
--      , [69,40,19, 6, 1, 2,11,28,53]
--      , [68,39,18, 5, 4, 3,12,29,54]
--      , [67,38,17,16,15,14,13,30,55]
--      , [66,37,36,35,34,33,32,31,56]
--      , [65,64,63,62,61,60,59,58,57]
--      ]
