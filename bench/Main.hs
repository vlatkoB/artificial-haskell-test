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
                                         mkWorldByTemplate, parseWorld, parseWorldConcurr)



critCfg :: Crit.Config
critCfg = defaultConfig {reportFile  = Just "artificial-haskell-test.html" }

scrollPath :: FilePath
scrollPath = "data/the.scroll"



main :: IO ()
main = do
  scroll <- BC.readFile scrollPath
  pw     <- parseWorldConcurr scroll

  let worldDimension = ceiling @Float . sqrt . fromIntegral $ length scroll
      spiralWorld    = force $ mkWorldByTemplate spiralWorldTemplate pw
      spiralCoords   = force $ addCoords spiralWorld
      directWorld    = force $ mkWorld pw

  defaultMainWith critCfg [
      bgroup "parsing" [
        bench "pure"     $ nfIO (pure $ parseWorld        scroll)
      , bench "parallel" $ nfIO (       parseWorldConcurr scroll)
      ]
    , bgroup "template" [
        bench "spiral"  $ nfIO (pure $ spiralWorldTemplate worldDimension)
      , bench "zig-zag" $ nfIO (pure $ zigZagWorldTemplate worldDimension)
      ]
    , bgroup "spiral world template" [
        bench "mkWorldTemplate" $ nfIO (pure $ mkWorldByTemplate spiralWorldTemplate pw)
      , bench "addCoords"       $ nfIO (pure $ addCoords spiralWorld)
      , bench "calcPopulation"  $ nfIO (pure $ findLargestIsland spiralCoords)
      ]
    , bgroup "world direct" [
        bench "mkWorld"        $ nfIO (pure $ mkWorld pw)
      , bench "max population" $ nfIO (pure $ findLargestIsland directWorld)
      ]
    , bgroup "finding coordinates" [
        bench "  100" $ nfIO (pure $ pointCoordInSpiral worldDimension   100)
      , bench " 1000" $ nfIO (pure $ pointCoordInSpiral worldDimension  1000)
      , bench "10000" $ nfIO (pure $ pointCoordInSpiral worldDimension 10000)
      , bench "90000" $ nfIO (pure $ pointCoordInSpiral worldDimension 90000)
      ]
    , bgroup "complete process" [
        bench "direct"   $ nfIO (maxByDirect   scroll)
      , bench "template" $ nfIO (maxByTemplate scroll)
      ]
    ]


maxByTemplate :: BC.ByteString -> IO Int
maxByTemplate bs = findLargestIsland
                .  addCoords
                .  mkWorldByTemplate spiralWorldTemplate
               <$> parseWorldConcurr bs

maxByDirect :: BC.ByteString -> IO Int
maxByDirect bs = findLargestIsland
              .  mkWorld
             <$> parseWorldConcurr bs
