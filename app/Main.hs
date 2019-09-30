module Main (main) where

import           ClassyPrelude

import           Control.Monad.ST      (runST)
import qualified Data.ByteString.Char8 as BC
import           Options.Applicative   (auto, execParser, flag', fullDesc, header, help,
                                        helper, info, long, option, optional, progDesc,
                                        short, showDefault, strOption, value, (<**>))
import           System.Directory      (doesFileExist, getFileSize)


import           Template              (spiralWorldTemplate, zigZagWorldTemplate)
import qualified Vector                as VecWorld
import           World                 (ProcessingKind (..), addCoords, findLargestIsland,
                                        mkWorld, mkWorldByTemplate, parseWorld)

-- | Scroll can be processed in these ways, which should be specified in cmd line
data ProcessType = Spiral        -- ^ use SpiralWorld template
                 | ZigZag        -- ^ use ZigZagWorld template
                 | NoTemplate    -- ^ find coordinates in SpiralWorld by algorithm
                 | Vector        -- ^ find coordinates in SpiralWorld by algorithm
  deriving (Show, Eq)


-- | Any cmd line arguments overrides config
data Args = Args
  { argScrollPath  :: FilePath    -- ^ config file to use
  , argProcessType :: ProcessType -- ^ how to process the scroll
  , argChunkSize   :: Maybe Int   -- ^ size of the chunk to perform concurrent parse
  } deriving (Show)

-- | Entry point
main :: IO ()
main = do
  Args{..} <- execParser pInfo

  putStrLn $ "\nProcessing '" <> pack argScrollPath <> "' scroll"
          <> maybe " sequentially" (const " in parallel") argChunkSize
          <> " via " <> tshow argProcessType
          <> " ..."

  doesFileExist argScrollPath >>= \case
    False -> putStrLn noFileError
    True  -> do
      size   <- fromIntegral <$> getFileSize argScrollPath
      scroll <- BC.readFile argScrollPath

      let pw       = parseWorld scroll
          procKind = case argChunkSize of
                       Nothing    -> Sequential
                       Just chunk -> Parallel chunk
          maxPop = case argProcessType of
                     Spiral     -> findLargestIsland
                                 . addCoords
                                 $ mkWorldByTemplate size spiralWorldTemplate pw
                     ZigZag     -> findLargestIsland
                                 . addCoords
                                 $ mkWorldByTemplate size zigZagWorldTemplate pw
                     NoTemplate -> findLargestIsland
                                 $ mkWorld size procKind pw
                     Vector     -> runST (VecWorld.parseAndProcess size scroll)

      putStrLn $ "Largest population on an island is: " <> tshow maxPop

  where
    pInfo = info (args <**> helper)
                 (fullDesc <> progDesc "Artificial haskell test"
                           <> header   "Artificial haskell test")
    args = Args <$> strOption (long "scroll"  <> short 's'
                            <> value "data/the.scroll" <> showDefault
                            <> help "Path of the scroll file")
                <*> (spiralP <|> zigZagP <|> noTemplateP <|> vectorP)
                <*> optional (option auto (long "chunk_size" <> short 'z'
                                        <> help "Size of chunk in bytes for concurrent parse")
                             )

    spiralP     = flag' Spiral     (long "spiral"      <> help "Spiral template")
    zigZagP     = flag' ZigZag     (long "zig-zag"     <> help "Zig-zag template")
    noTemplateP = flag' NoTemplate (long "no-template" <> help "No template, Spiral")
    vectorP     = flag' Vector     (long "by-vector"   <> help "Using vector, no coordinates, Spiral")


noFileError :: Text
noFileError = "Specify scroll path or ensure 'data/the.scroll' exists"
