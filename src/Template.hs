module Template where

import ClassyPrelude
import Data.List     (transpose)


-- | Shape how templates are represented
type Template  = [[Int]]

-- | Create a spiral template starting in the middle and going clock-wise
spiralWorldTemplate :: Int -> Template
spiralWorldTemplate 1 = singleton (singleton 1)
spiralWorldTemplate n =
  if odd n
    then oddRow <> zipWith (<>) (transpose $ map reverse cols) smallerTemplate
    else           zipWith (<>) smallerTemplate (transpose cols <> evenRow)    <> evenRow

  where
    evenRow         = [reverse row]
    oddRow          = [row]
    pow2            =  (^ (2 :: Int))
    smallerTemplate = spiralWorldTemplate $ pred n
    newElems        = [succ (pow2 (pred n)) .. pow2 n]
    (cols,row)      = first singleton $ splitAt (length newElems `div` 2) newElems

-- | Create a zig-zag template starting left-top and going down, like snake
zigZagWorldTemplate :: Int -> Template
zigZagWorldTemplate n = map mkList $ [0..n - 1]
  where mkList i = (if odd i then reverse else id) $ [(i * n + 1).. (i + 1) * n]


tst :: MonadIO m => Int -> m ()
tst n = do
  -- let t = toList $ map toList $ spiralWorldTemplate n
  let t = toList $ map toList $ zigZagWorldTemplate n
  mapM_ (print . map ss) t

  where
    ss n1 = do
      let a = show n1
      if length a < 2 then " " <> a else a
