module Vector
  ( parseAndProcess
  , parseWorld
  , getMaxPopulation
  )
where

import           ClassyPrelude

import qualified Data.ByteString.Char8       as BC
import           Data.Char                   (chr)
import           Data.Foldable               (foldrM)
import qualified Data.IntSet                 as S
import qualified Data.Vector.Unboxed.Mutable as MV

import           Template                    (Coord, coordToSeqSpiral, seqToCoordSpiral)


------------------------------------------------------------------------------------------
-- Types
------------------------------------------------------------------------------------------

newtype VecIdx = VecIdx {unVecIdx ::  Int} deriving (NFData)
instance Show VecIdx where show (VecIdx i) = '#' : show i


------------------------------------------------------------------------------------------
-- Aliases
------------------------------------------------------------------------------------------

type Population = Word8
type MVec m     = MV.MVector (PrimState m) Population
type VecWorld m = (PrimMonad m)


------------------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------------------

water :: Population
water = 0

------------------------------------------------------------------------------------------
-- Funcs
------------------------------------------------------------------------------------------

-- | Like `parseWorld`, but creates mutable `Vector`, instead of string
parseWorld :: forall m. VecWorld m => Int -> BC.ByteString -> m (IntSet, MVec m)
parseWorld sz bs = do
  vec <- MV.replicate (sz + 1) water
  vs  <- snd <$> foldlM (countWatersVec vec) ((VecIdx 1,0),mempty) bs
  pure (vs,vec)

  where
    -- | Count the waters and make population, or transform 0 water # to water
    --   Write to vector only if population is larger than zero
    countWatersVec :: MVec m -> ((VecIdx,Population),IntSet) -> Word8
                   -> m ((VecIdx,Population),IntSet)
    countWatersVec vec ((VecIdx !i,!n),!vs) c = case (n, chr $ fromIntegral c) of
      (0,'#') ->                     ret 0                                    vs
      (_,'~') ->                     ret (bool 0 (succ n) $ n < 9)            vs
      (_,'#') -> MV.write vec i n >> ret 0                        (S.insert i vs)
      (_,x)   -> error $ "Bad char in scroll: " <> [x]
      where
        ret pop = pure . ((VecIdx $ succ i, pop),)

parseAndProcess :: forall m. VecWorld m => Int -> BC.ByteString -> m Int
parseAndProcess sz bs = do
  let dim = floor @Float . sqrt $ fromIntegral sz
  getMaxPopulation dim =<< parseWorld sz bs



getMaxPopulation :: forall m. VecWorld m => Int -> (IntSet,MVec m) -> m Int
getMaxPopulation dim (!vlgPositions,vec) = getNextVlg vlgPositions >>= \case
  Nothing                  -> pure 0
  Just (pos,vlgPositions') -> do
    (currPopulation, vsWithoutIsland) <- mkIsland vlgPositions' pos
    nextPopulation                    <- getMaxPopulation dim (vsWithoutIsland,vec)
    pure $ max currPopulation nextPopulation

  where
    -- Make an island around the village
    mkIsland :: IntSet -> VecIdx -> m (Int,IntSet)
    mkIsland s vlg = do
      let sWithoutCurr = S.delete (unVecIdx vlg) s
      vlgPop <- fromIntegral <$> getPopulation vlg
      delVillage vlg
      foldrM getHoodPopulation (vlgPop,sWithoutCurr) $ hoodIndexes vlg

    -- Find touching neighbours and sum their population
    getHoodPopulation :: VecIdx -> (Int,IntSet) -> m (Int,IntSet)
    getHoodPopulation vlg (islandPop,s) = isWater vlg >>= \case
      True  -> pure (islandPop,s)
      False -> first (islandPop +) <$> mkIsland s vlg

    -- Find next village in the ocean using the set of all village indexes
    getNextVlg :: IntSet -> m (Maybe (VecIdx, IntSet))
    getNextVlg s
      | S.null s = pure Nothing
      | otherwise = do
          let (v,vs) = S.deleteFindMin s
          isWater (VecIdx v) >>= \case
            True  -> getNextVlg vs -- if deleted as part of an island
            False -> pure $ Just (VecIdx v, vs)

    -- Check if position is water
    isWater :: VecIdx -> m Bool
    isWater vlg = (water ==) <$> getPopulation vlg

    -- Get positions of touching neighbours
    hoodIndexes :: VecIdx -> [VecIdx]
    hoodIndexes (VecIdx pos) =
      if pos > maxPos
        then []
        else mapMaybe getPosition . surroundCoords $ seqToCoordSpiral dim pos
      where
        {-# INLINE getPosition #-}
        getPosition (x,y) = if x <= dim && y <= dim
          then Just . VecIdx $ coordToSeqSpiral dim (x,y)
          else Nothing


    maxPos = dim * dim

    {-# INLINE delVillage #-}
    delVillage :: VecIdx -> m ()
    delVillage (VecIdx i) =  if i > MV.length vec then pure () else MV.write vec i water

    {-# INLINE getPopulation #-}
    getPopulation :: VecIdx -> m Population
    getPopulation (VecIdx i) = if i > MV.length vec then pure 0 else MV.read vec i


-- | Get matrix of surrounding points of given point, a "circle" around it
surroundCoords :: Coord -> [Coord]
surroundCoords (x,y) = filter (\(x',y') -> x' > 0 && y' > 0)
  [ (x-1, y-1),(x, y-1),(x+1, y-1)
  , (x-1, y  ),         (x+1, y)
  , (x-1, y+1),(x, y+1),(x+1, y+1)
  ]
