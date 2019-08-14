module World
  ( World
  , VillageXY
  , VillagesMap
  , ParsedWorld (..)

  , calcIslandPopulation
  , parseWorld
  , mkWorldByTemplate
  , addCoords
  , mkIslands
  )
where

import ClassyPrelude
import Data.List     (nub)

import Template      (Template)


------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------

-- | World representation after parsed scroll has been mapped over a 'Template'
type World     = [String]

-- | Population of the village with its coordinates on the map
type VillageXY = (Char, (Int, Int))

-- | Map of the world containing only villages with their population and coordinates
--   Water is not present here
type VillagesMap  = IntMap [VillageXY]

------------------------------------------------------------------------------------------
-- Data
------------------------------------------------------------------------------------------

-- | Hold a scroll with parsed population
newtype ParsedWorld = ParsedWorld String
  deriving (Show, Eq, Ord)


------------------------------------------------------------------------------------------
-- Operation
------------------------------------------------------------------------------------------

-- | Sum up the population of each village on every island
calcIslandPopulation :: [[VillageXY]] -> [Int]
calcIslandPopulation = map (sum . mapMaybe ( readMay . (:[]) . fst))


-- | Parse the scroll to find out which population each village has
--   - # can be water or village
--   - # without population is water
--   - ~ is water
--   - number of ~ before # signifies the population of the village
--   - if population larger that 9, use last digit as population
parseWorld :: String -> ParsedWorld
parseWorld = ParsedWorld . reverse . snd . foldl' go (0 :: Int, mempty)
  where
    go (population,acc) c = do
      let lastDigit = readMay . take 1 . reverse $ show population
      case (lastDigit, c) of
        (Just 0,'#') -> (0,   '~' : acc)
        (Just n,'~') -> (n+1, '~' : acc)
        (Just n,'#') -> (0,   show n <> acc)
        _            -> error "parseWorld: impossible happened"


-- | Roll flatten parsed world population over the specified template,
--   like spiralWorldTemplate
mapOver :: String -> Template -> World
mapOver s = map (map go)
  where go n = fromMaybe ' ' . headMay . take 1 $ drop (pred n) s


-- | Create a 2D world according to specified `Template` and map parsed scroll with
--   population over it
mkWorldByTemplate :: (Int -> Template) -> ParsedWorld -> World
mkWorldByTemplate template (ParsedWorld parsedWorld) =
  parsedWorld `mapOver` ( toList
                        . map toList
                        . template
                        . ceiling @Float
                        . sqrt
                        . fromIntegral
                        $ length parsedWorld)


-- | Add to each village its coordinates and after remove any water
addCoords :: World -> VillagesMap
addCoords =
  mapFromList . map (\(row,cs) -> (row, mapMaybe (go row) cs))
              . zip [1..]
              . map (zip [1..])
  where
    go _ (_,'~') = Nothing
    go x (y,v)   = Just (v,(x,y))


-- | Find connected villages and group them as islands
mkIslands :: VillagesMap -> [[VillageXY]]
mkIslands wm = snd . foldl' isolateVillage ([],[]) $ concat (toList wm)

  where
    -- isolateVillage (ign,acc) = second (\a -> bool (acc <> [a]) acc $ null a) . go ign []
    isolateVillage (ign,acc) = second (\a -> bool (a : acc) acc $ null a) . go ign []
    adjucentRows m n = fromMaybe [] (lookup (n-1) m)
                    <> fromMaybe [] (lookup n m)
                    <> fromMaybe [] (lookup (n+1) m)

    go ignore acc vlg@(_,(x,y)) =
      if vlg `elem` ignore
        then (ignore,acc)
        else do
          let vs = filter (\vxy@(_,(_,y')) -> vxy `notElem` (vlg : ignore)
                                           && (y' >= pred y && y' <= succ y)
                                              --`elem` [pred y .. succ y]
                          )
                     $ adjucentRows wm x
          case vs of
            [] -> (vlg : ignore, nub $ acc <> [vlg])
            _  -> concatMap (go (vlg : ignore) (vlg : vs <> acc)) vs
