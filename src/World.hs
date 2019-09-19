module World
  ( World (..)
  , Village
  , ParsedWorld
  , parsedWorldEqualString

  , parseWorld
  , findLargestIsland
  , mkWorldByTemplate
  , mkWorld
  , addCoords
  )
where

import           ClassyPrelude

import qualified Data.ByteString.Char8 as BC
import           Data.Char             (isDigit, ord)
import qualified Data.HashMap.Strict   as M
import           GHC.Generics          (Generic)

import           Template              (Coord, Template (..), mapOver, pointCoordInSpiral)

------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------

-- | World representation after parsed scroll has been mapped over a 'Template'
newtype World = World {unWorld :: [BC.ByteString] }
  deriving (Show, Eq, Ord, Generic)

-- | Hold a scroll with parsed population
newtype ParsedWorld = ParsedWorld {unParsedWorld :: String }
  deriving (Show, Eq, Ord, Generic)


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
-- Testing helpers
------------------------------------------------------------------------------------------

-- | Data constructor not exported, so this one allows comparing parsed world with string
parsedWorldEqualString :: ParsedWorld -> String -> Bool
parsedWorldEqualString (ParsedWorld a) b = a == b


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
parseWorld :: Int -> BC.ByteString -> IO ParsedWorld
parseWorld chunkSize = fmap (ParsedWorld . concatMap (reverse . snd))
                     . mapConcurrently (pure . BC.foldl' countWaters (0, mempty))
                     . chunks chunkSize
  where
    chunks n xs
      | null xs   = mempty
      | otherwise = let (ys, zs) = BC.splitAt n xs in ys : chunks n zs

    -- | Count the waters and make population, or transform 0 water # to water
    countWaters :: (Int, String) -> Char -> (Int, String)
    countWaters (population,acc) c = do
      let lastDigit = take 1 . reverse $ show population
      case (readMay lastDigit, headMay lastDigit, c) of
        (Just 0, _,     '#') -> (0,   '~' : acc)
        (Just n, _,     '~') -> (n+1, '~' : acc)
        (Just _, Just d,'#') -> (0,   d   : acc)
        _                    -> error "countWaters: impossible happened"


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
mkWorldByTemplate :: (Int -> Template) -> ParsedWorld -> World
mkWorldByTemplate template (ParsedWorld parsedWorld) =
  World $ BC.pack parsedWorld `mapOver` ( template
                                        . ceiling @Float
                                        . sqrt
                                        . fromIntegral
                                        $ length parsedWorld)


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
mkWorld :: ParsedWorld -> VillageCoords
mkWorld (ParsedWorld parsedWorld) =
  mapFromList . mapMaybe go $ zip [1..] parsedWorld

  where
    dim       = ceiling @Float . sqrt . fromIntegral $ length parsedWorld
    go (n, c) = if isDigit c
      then Just . (,c) $ pointCoordInSpiral dim n
      else Nothing
