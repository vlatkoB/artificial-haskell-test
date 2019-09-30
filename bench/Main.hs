module Main where

import           ClassyPrelude          hiding (lookup)

import           Criterion.Main         (defaultMainWith)
import           Criterion.Main.Options (defaultConfig)
import           Criterion.Types        as Crit
import qualified Data.ByteString.Char8  as BC
import           System.Directory       (getFileSize)


import           Orphans                ()
import           Template               (coordToSeqSpiral, seqToCoordSpiral,
                                         spiralWorldTemplate, zigZagWorldTemplate)
import qualified Vector                 as VecWorld
import           World                  (ProcessingKind (..), addCoords,
                                         findLargestIsland, mkWorld, mkWorldByTemplate,
                                         parseWorld)



critCfg :: Crit.Config
critCfg = defaultConfig {reportFile  = Just "artificial-haskell-test-bench.html" }

scrollPath :: FilePath
scrollPath = "data/the.scroll"

main :: IO ()
main = do
  sz     <- fromIntegral <$> getFileSize scrollPath
  scroll <- BC.readFile scrollPath
  vsVec  <- force <$> VecWorld.parseWorld sz scroll

  let !pw             = parseWorld scroll
      !worldDimension = ceiling @Float . sqrt $ fromIntegral sz
      !spiralWorld    = force $ mkWorldByTemplate sz spiralWorldTemplate pw
      !spiralCoords   = force $ addCoords spiralWorld
      !directWorld    = force $ mkWorld sz Sequential pw

  defaultMainWith critCfg [
      bgroup "parsing" [
        bench "sequential" $ nfIO (pure $      parseWorld    scroll)
      , bench "vector"     $ nfIO (   VecWorld.parseWorld sz scroll)
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
        bench "mkWorld"            $ nfIO (pure $ mkWorld sz Sequential      pw)
      , bench "mkWorldPar"         $ nfIO (pure $ mkWorld sz (Parallel 3000) pw)
      ]
    , bgroup "max population spiral" [
        bench "no template" $ nfIO (pure $ findLargestIsland directWorld)
      , bench "vector"      $ nfIO (       VecWorld.getMaxPopulation worldDimension vsVec)
      ]
    , bgroup "sequential to coordinates" [
        bench "      100" $ nfIO (pure $ seqToCoordSpiral worldDimension       100)
      , bench "    1 000" $ nfIO (pure $ seqToCoordSpiral worldDimension     1_000)
      , bench "   10 000" $ nfIO (pure $ seqToCoordSpiral worldDimension    10_000)
      , bench "4 000 000" $ nfIO (pure $ seqToCoordSpiral           3000 4_000_000)
      ]
    , bgroup "coordinates to sequential" [
        bench " 1 000" $ nfIO (pure $ coordToSeqSpiral  1_000 (   200,    200))
      , bench " 5 000" $ nfIO (pure $ coordToSeqSpiral  5_000 ( 2_000,  2_000))
      , bench "10 000" $ nfIO (pure $ coordToSeqSpiral 10_000 ( 7_000,  7_000))
      , bench "20 000" $ nfIO (pure $ coordToSeqSpiral 20_000 (13_000, 13_000))
      ]
    , bgroup "complete process" [
        bench "NO template"     $ nfIO (pure $ maxByNoTemplate  Sequential     sz scroll)
      , bench "NO template PAR" $ nfIO (pure $ maxByNoTemplate (Parallel 3000) sz scroll)
      , bench "NO template VEC" $ nfIO (       maxByVector                     sz scroll)
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

maxByVector :: Int -> BC.ByteString -> IO Int
maxByVector = VecWorld.parseAndProcess
