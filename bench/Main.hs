{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import           ClassyPrelude          hiding (lookup)
import           Criterion.Main         (defaultMainWith)
import           Criterion.Main.Options (defaultConfig)
import           Criterion.Types        as Crit
import qualified Data.ByteString.Char8  as BC


import           Orphans                ()
import           Template               (pointCoordInSpiral, spiralWorldTemplate,
                                         zigZagWorldTemplate)
import           World                  (addCoords, findLargestIsland, mkWorld,
                                         mkWorldByTemplate, parseWorld)



critCfg :: Crit.Config
critCfg = defaultConfig {reportFile  = Just "artificial-haskell-test.html" }

scrollPath :: FilePath
scrollPath = "data/the.scroll"

chunkSize :: Int
chunkSize = 3000



main :: IO ()
main = do
  scroll <- BC.readFile scrollPath
  pw     <- parseWorld chunkSize scroll

  let worldDimension = ceiling @Float . sqrt . fromIntegral $ length scroll
      spiralWorld    = force $ mkWorldByTemplate spiralWorldTemplate pw
      spiralCoords   = force $ addCoords spiralWorld
      directWorld    = force $ mkWorld pw

  defaultMainWith critCfg [
      bgroup "parsing" [
        bench "concurrent" $ nfIO (parseWorld chunkSize scroll)
      ]
    , bgroup "make template" [
        bench "spiral"  $ nfIO (pure $ spiralWorldTemplate worldDimension)
      , bench "zig-zag" $ nfIO (pure $ zigZagWorldTemplate worldDimension)
      ]
    , bgroup "spiral world template" [
        bench "mkWorldTemplate" $ nfIO (pure $ mkWorldByTemplate spiralWorldTemplate pw)
      , bench "addCoords"       $ nfIO (pure $ addCoords spiralWorld)
      , bench "calcPopulation"  $ nfIO (pure $ findLargestIsland spiralCoords)
      ]
    , bgroup "world NO template" [
        bench "mkWorld"        $ nfIO (pure $ mkWorld pw)
      , bench "max population" $ nfIO (pure $ findLargestIsland directWorld)
      ]
    , bgroup "finding coordinates" [
        bench "      100" $ nfIO (pure $ pointCoordInSpiral worldDimension       100)
      , bench "    1 000" $ nfIO (pure $ pointCoordInSpiral worldDimension     1_000)
      , bench "   10 000" $ nfIO (pure $ pointCoordInSpiral worldDimension    10_000)
      , bench "4 000 000" $ nfIO (pure $ pointCoordInSpiral           3000 4_000_000)
      ]
    , bgroup "complete process" [
        bench "NO template" $ nfIO (maxByNoTemplate scroll)
      , bench "template"    $ nfIO (maxByTemplate   scroll)
      ]
    ]


maxByTemplate :: BC.ByteString -> IO Int
maxByTemplate bs =  findLargestIsland
                 .  addCoords
                 .  mkWorldByTemplate spiralWorldTemplate
                <$> parseWorld chunkSize bs

maxByNoTemplate :: BC.ByteString -> IO Int
maxByNoTemplate bs =  findLargestIsland
                   .  mkWorld
                  <$> parseWorld chunkSize bs
