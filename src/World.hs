module World
  ( World (..)
  , Village
  , ParsedWorld
  , unParsedWorld
  , ProcessingKind (..)

  , parseWorld
  , findLargestIsland
  , mkWorldByTemplate
  , mkWorld
  , addCoords
  )
where

import           ClassyPrelude

import           Control.Parallel.Strategies (using, parListChunk, rdeepseq)
import qualified Data.ByteString.Char8       as BC
import           Data.Char                   (chr, isDigit, ord)
import qualified Data.HashMap.Strict         as M
import           GHC.Generics                (Generic)

import           Template                    (Coord, Template (..), mapOver,
                                              pointCoordInSpiral)


------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------

-- | World representation after parsed scroll has been mapped over a 'Template'
newtype World = World {unWorld :: [BC.ByteString] }
  deriving (Show, Eq, Ord, Generic)

-- | Hold a scroll with parsed population
newtype ParsedWorld = ParsedWorld String
  deriving (Show, Eq, Ord, Generic)

unParsedWorld :: ParsedWorld -> String
unParsedWorld (ParsedWorld s) = s

-- | How to process the scroll (only mkWorld for now)
data ProcessingKind = Sequential | Parallel Int

------------------------------------------------------------------------------------------
-- Aliases
------------------------------------------------------------------------------------------
-- | Population of the village with its coordinates on the map
type Village = (Coord, Char)

-- | Map of the world containing only villages with their population and coordinates
--   Water is not present here
type VillageCoords = HashMap Coord Char

-- | Group of villages for an island
type Island = [Village]


------------------------------------------------------------------------------------------
-- Funcs
------------------------------------------------------------------------------------------

-- | Sum up the population of 1 island
islandPopulation :: Island -> Int
islandPopulation = sum . map (toInt . snd)
  where toInt c = ord c - 48


-- | Parse the scroll to find out which population each village has
--   - # can be water or village
--   - # without population is water
--   - ~ is water
--   - number of ~ before # signifies the population of the village
--   - if population larger that 9, use last digit as population
parseWorld :: BC.ByteString -> ParsedWorld
parseWorld = ParsedWorld . snd . BC.foldl' countWaters (0, mempty)
  where
    -- | Count the waters and make population, or transform 0 water # to water
    countWaters :: (Int, String) -> Char -> (Int, String)
    countWaters (n,acc) c = do
      let succN = bool 0 (n + 1) $ n < 9
          charN = chr $ n + 48
      case (n, c) of
        (0, '#') -> (0,     '~'   : acc)
        (_, '~') -> (succN, '~'   : acc)
        (_, '#') -> (0,     charN : acc)
        (_, x)   -> error $ "Bad char in scroll: " <> [x]


-- | Given the coordinates and population of each village, group villages into islands,
--   count the population of each island and return the largest
findLargestIsland :: VillageCoords -> Int
findLargestIsland villageCoords = case getFirstVlg villageCoords of
  Nothing  -> 0
  Just vlg -> do
    let (currIsland, newVC) = mkIsland villageCoords vlg
    max (islandPopulation currIsland)
      . bool (findLargestIsland newVC) 0
      $ null newVC

  where
    mkIsland !vc' vlg =
      let vc = M.delete (fst vlg) vc'
       in first (vlg :) . foldr getHood ([], vc)
                        . mapMaybe (\xy -> (xy,) <$> lookup xy vc)
                        $ surroundCoords $ fst vlg

    getHood !vlg (!accIslands, !accVC) = case M.lookup (fst vlg) accVC of
      Nothing -> (accIslands, accVC)
      Just _  -> first (accIslands <>) $ mkIsland accVC vlg

    getFirstVlg m = case M.keys m of
      (k:_) -> (k,) <$> lookup k m
      _     -> Nothing


-- | Get matrix of surrounding points of given point, a "circle" around it
surroundCoords :: (Int, Int) -> [(Int, Int)]
surroundCoords (x,y) = [ (x-1, y-1),(x, y-1),(x+1, y-1)
                       , (x-1, y  ),         (x+1, y)
                       , (x-1, y+1),(x, y+1),(x+1, y+1)
                       ]


------------------------------------------------------------------------------------------
-- Template related functions
------------------------------------------------------------------------------------------

-- | Create a 2D world according to specified `Template` and map parsed scroll with
--   population over it. It is a left-top matrix.
mkWorldByTemplate :: Int -> (Int -> Template) -> ParsedWorld -> World
mkWorldByTemplate size template (ParsedWorld parsedWorld) = World $
  BC.pack (reverse parsedWorld) `mapOver` ( template
                                          . ceiling @Float
                                          . sqrt
                                          $ fromIntegral size)


-- | Add to each village its coordinates and after remove any water
addCoords :: World -> VillageCoords
addCoords = mapFromList . concatMap (\(x,yv) -> mapMaybe (go x) yv)
                        . zip [1..]                   -- add Y coordinate
                        . map (zip [1..] . BC.unpack) -- add X coordinate
                        . unWorld
  where
    go _ (_,'~') = Nothing
    go y (x,v)   = Just ((x, y) ,v)


------------------------------------------------------------------------------------------
-- No-template related functions
------------------------------------------------------------------------------------------

-- | Gather villages and their coordinates only in a map.
mkWorld :: Int -> ProcessingKind -> ParsedWorld -> VillageCoords
mkWorld size pk (ParsedWorld parsedWorld) = do
  let xs = zip [ size, size - 1 .. 1] parsedWorld
  mapFromList $ case pk of
    Sequential     -> mapMaybe go xs
    Parallel chunk -> (mapMaybe go) xs
                        `using`
                      parListChunk chunk rdeepseq

  where
    dim       = ceiling @Float . sqrt $ fromIntegral size
    go (n, c) = if isDigit c
      then Just . (,c) $ pointCoordInSpiral dim $ n
      else Nothing
