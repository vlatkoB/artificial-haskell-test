module Main (main) where

import           ClassyPrelude

import qualified Data.ByteString.Char8 as BC
import           Options.Applicative   (auto, execParser, flag', fullDesc, header, help,
                                        helper, info, long, option, optional, progDesc,
                                        short, showDefault, strOption, value, (<**>))
import           System.Directory      (doesFileExist, getFileSize)


import           Template              (spiralWorldTemplate, zigZagWorldTemplate)
import           World                 (ProcessingKind (..), addCoords, findLargestIsland,
                                        mkWorld, mkWorldByTemplate, parseWorld)

-- | Scroll can be processed in these ways, which should be specified in cmd line
data ProcessType = Spiral        -- ^ use SpiralWorld template
                 | ZigZag        -- ^ use ZigZagWorld template
                 | NoTemplate    -- ^ find coordinates in SpiralWorld by algorithm
  deriving (Show)


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
      size <- fromIntegral <$> getFileSize argScrollPath
      pw   <- parseWorld <$> BC.readFile argScrollPath

      let procKind = case argChunkSize of
                       Nothing    -> Sequential
                       Just chunk -> Parallel chunk
          vlgs = case argProcessType of
                   Spiral     -> addCoords $ mkWorldByTemplate size spiralWorldTemplate pw
                   ZigZag     -> addCoords $ mkWorldByTemplate size zigZagWorldTemplate pw
                   NoTemplate -> mkWorld size procKind pw

      putStrLn $ "Largest population on an island is: " <> tshow (findLargestIsland vlgs)

  where
    pInfo = info (args <**> helper)
                 (fullDesc <> progDesc "Artificial haskell test"
                           <> header   "Artificial haskell test")
    args = Args <$> strOption (long "scroll"  <> short 's'
                            <> value "data/the.scroll" <> showDefault
                            <> help "Path of the scroll file")
                <*> (spiralP <|> zigZagP <|> noTemplateP)
                <*> optional (option auto (long "chunk_size" <> short 'z'
                                        <> help "Size of chunk in bytes for concurrent parse")
                             )

    spiralP     = flag' Spiral     (long "spiral"      <> help "Spiral template")
    zigZagP     = flag' ZigZag     (long "zig-zag"     <> help "Zig-zag template")
    noTemplateP = flag' NoTemplate (long "no-template" <> help "No template, Spiral")


noFileError :: Text
noFileError = "Specify scroll path or ensure 'data/the.scroll' exists"
