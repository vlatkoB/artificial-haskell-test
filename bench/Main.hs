{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           ClassyPrelude          hiding (lookup)
import           Criterion.Main         (defaultMainWith)
import           Criterion.Main.Options (defaultConfig)
import           Criterion.Types        as Crit
import qualified Data.ByteString.Char8  as BC
import           System.Directory       (getFileSize)


import           Orphans                ()
import           Template               (pointCoordInSpiral, spiralWorldTemplate,
                                         zigZagWorldTemplate)
import           World                  (ProcessingKind (..), addCoords,
                                         findLargestIsland, mkWorld, mkWorldByTemplate,
                                         parseWorld)



critCfg :: Crit.Config
critCfg = defaultConfig {reportFile  = Just "artificial-haskell-test.html" }

scrollPath :: FilePath
scrollPath = "data/the.scroll"

main :: IO ()
main = do
  sz     <- fromIntegral <$> getFileSize scrollPath
  scroll <- BC.readFile scrollPath

  let !pw             = parseWorld scroll
      !worldDimension = ceiling @Float . sqrt $ fromIntegral sz
      !spiralWorld    = force $ mkWorldByTemplate sz spiralWorldTemplate pw
      !spiralCoords   = force $ addCoords spiralWorld
      !directWorld    = force $ mkWorld sz Sequential pw

  defaultMainWith critCfg [
      bgroup "parsing" [
        bench "sequential" $ nfIO (pure $ parseWorld scroll)
      ]
    , bgroup "make template" [
        bench "spiral"  $ nfIO (pure $ spiralWorldTemplate worldDimension)
      , bench "zig-zag" $ nfIO (pure $ zigZagWorldTemplate worldDimension)
      ]
    , bgroup "spiral world template" [
        bench "mkWorldTemplate" $ nfIO (pure $ mkWorldByTemplate sz spiralWorldTemplate pw)
      , bench "addCoords"       $ nfIO (pure $ addCoords spiralWorld)
      , bench "calcPopulation"  $ nfIO (pure $ findLargestIsland spiralCoords)
      ]
    , bgroup "world NO template" [
        bench "mkWorld"        $ nfIO (pure $ mkWorld sz Sequential      pw)
      , bench "mkWorldPar"     $ nfIO (pure $ mkWorld sz (Parallel 3000) pw)
      , bench "max population" $ nfIO (pure $ findLargestIsland directWorld)
      ]
    , bgroup "finding coordinates" [
        bench "      100" $ nfIO (pure $ pointCoordInSpiral worldDimension       100)
      , bench "    1 000" $ nfIO (pure $ pointCoordInSpiral worldDimension     1_000)
      , bench "   10 000" $ nfIO (pure $ pointCoordInSpiral worldDimension    10_000)
      , bench "4 000 000" $ nfIO (pure $ pointCoordInSpiral           3000 4_000_000)
      ]
    , bgroup "complete process" [
        bench "NO template"     $ nfIO (pure $ maxByNoTemplate  Sequential     sz scroll)
      , bench "NO template PAR" $ nfIO (pure $ maxByNoTemplate (Parallel 3000) sz scroll)
      , bench "template"        $ nfIO (pure $ maxByTemplate                   sz scroll)
      ]
    ]


maxByTemplate :: Int -> BC.ByteString -> Int
maxByTemplate sz =  findLargestIsland
                 .  addCoords
                 .  mkWorldByTemplate sz spiralWorldTemplate
                 .  parseWorld

maxByNoTemplate :: ProcessingKind -> Int -> BC.ByteString -> Int
maxByNoTemplate pk sz = findLargestIsland
                      . mkWorld sz pk
                      . parseWorld
