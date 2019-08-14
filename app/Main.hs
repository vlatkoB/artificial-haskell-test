module Main (main) where

import ClassyPrelude    hiding (readFile)
import Data.String.Conv (toS)
import System.Directory (doesFileExist)
import System.IO        (readFile)

import Template         (spiralWorldTemplate)
import World            (addCoords, calcIslandPopulation, mkWorldByTemplate, parseWorld,
                         mkIslands)


-- testScroll :: String
-- testScroll = "#~~~##~~#~~###~#~~##~#~#~"

main :: IO ()
main = do
  fNm <- getArgs <&> \case
    (fNm:_) -> toS fNm
    _       -> "data/the.scroll"

  b <- doesFileExist fNm
  unless b $ error
    "Specify scroll filename as param to this app or ensure 'data/the.scroll' exists"

  x <-
     --   calcIslandPopulation
       mkIslands
    -- length
     .  addCoords
     . mkWorldByTemplate spiralWorldTemplate
     .  parseWorld
    <$> readFile fNm
    -- <$> pure testScroll
  print x
  pure ()


-- getIt :: MonadIO m => String -> m ()
-- getIt scroll = do
--   let (spiralWorld :: World) = mkWorldByTemplate spiralWorldTemplate scroll
--       vxy = addCoords spiralWorld
--       vlgs = mkIslands vxy
--   mapM_ (putStrLn . toS) spiralWorld

--   putStr "DXY: " >> print vxy
--   putStr "VLG: " >> print vlgs
--   putStrLn "\nRESULT"
--   mapM_ print vlgs
