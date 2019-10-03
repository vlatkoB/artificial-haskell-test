module Template
  ( Template (..)
  , Coord

  , mapOver
  , seqToCoordSpiral
  , coordToSeqSpiral
  , spiralWorldTemplate
  , zigZagWorldTemplate
  )
where

import ClassyPrelude

import Data.List                    (transpose)
import GHC.Generics                 (Generic)


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
mapOver :: [Word8] -> Template -> [[Word8]]
mapOver s = map (map go) . unTemplate
  where go n = maybe 0 fst . uncons $ drop (pred n) s

-- | Get the coordinates for a sequential point in an n-dimensional 2D square spiral.
--   Coord system is top-left based starting with (1,1)
seqToCoordSpiral :: Int -> Int -> Coord
seqToCoordSpiral 1 1 = (1,1)
seqToCoordSpiral 2 p =
  case p of
    1 -> ( 1, 1)
    2 -> ( 2, 1)
    3 -> ( 2, 2)
    4 -> ( 1, 2)
    _ -> (-1,-1)
seqToCoordSpiral dimension position = do
  let fullDim = floor   @Float . sqrt $ fromIntegral position
      jumpDim = ceiling @Float $ fromIntegral (dimension - fullDim) / 2
      initPos = ((jumpDim +) *** (jumpDim +)) $
        if | even fullDim   -> (1,           fullDim)
           | even dimension -> (fullDim - 1, 0)
           | otherwise      -> (fullDim,     1)
      newPos  = position - pow2 fullDim

  if position <= pow2 dimension
    then go (fullDim + 1) newPos initPos
    else error $ "Matrix dimension (" <> show dimension <> ") "
              <> "too small for searched point (" <> show position <> ")"
  where
    go !dim n (!x, !y) =
      if | n <= 0    -> (x, y)
         | even dim  -> (x + 1 - xMove, y + yMove)
         | odd  dim  -> (x - 1 + xMove, y - yMove)
         | otherwise -> error "seqToCoordSpiral: go >> should never be reached"
      where
        yMove = min (dim - 1) (n - 1)
        xMove = min (dim - 1) (n - 1 - yMove)


-- | Convert coordinates from specified dimension to sequential number in Spiral world
coordToSeqSpiral :: Int -> Coord -> Int
coordToSeqSpiral dim (x', y') = do
  let mp = ceiling @Float @Int $ fromIntegral dim / 2
      x  = x' - mp
      y  = mp - y'
  1 + if |       y  >=   abs x  -> 4 * pow2 y + 3 * y + x
         |       y  <= -(abs x) -> 4 * pow2 y +     y - x
         |   abs y  <        x  -> 4 * pow2 x - 3 * x - y
         | -(abs y) >=       x  -> 4 * pow2 x -     x + y
         | otherwise            -> -1


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
