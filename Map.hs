{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}

module Map where

import           Control.Monad
import           Control.Monad.Trans.State
import           Data.IORef
import           Data.Coerce
import           Data.List (sortOn, unfoldr, sort, nubBy)
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import           System.IO (Handle)
import           System.Random
import qualified Data.Binary as B
import           GHC.Generics
import qualified Data.ByteString.Lazy as BL
import           Data.ByteString.Builder

import           Lib

type Point = (Int, Int)

type PPlanet = (Point, Planet)

randomCoordinate :: Int -> IO Point
randomCoordinate radius
  = (,) <$> randomRIO (0, radius) <*> randomRIO (-radius, radius)

-- No reason for this to be in IO, but it is
tooClose :: [PPlanet] -> Point -> Growth -> IO Bool
tooClose ps p (Growth g) = do
  let multiplier = 0.9
  cs <- forM ps $ \ (p', Planet _ _ (Growth g')) -> do
    let
        dist = distance p p'
        threshold = multiplier * (sqrt (fromIntegral g) + sqrt (fromIntegral g'))
    return $ dist < threshold || dist <= 5
  return (any id cs)

randomPlanet :: Int -> [PPlanet] -> IO PPlanet
randomPlanet radius ps = do
  c      <- randomCoordinate radius
  ships  <- Ships <$> randomRIO (50,150)
  growth <- Growth <$> randomRIO (1,5)
  too_close <- tooClose ps c growth
  if not too_close && inMap radius c then do
    return (c, Planet Neutral ships growth)
  else randomPlanet radius ps

mirrorPlanet :: PPlanet -> PPlanet
mirrorPlanet ((x, y), Planet o s g)
  = ((-x, -y), Planet (mirrorOwner o) s g)
  where mirrorOwner Neutral = Neutral
        mirrorOwner (Owned Player1) = Owned Player2
        mirrorOwner (Owned Player2) = Owned Player1

-- generates random planets for player 1
randomPlanets :: Int -> IO (PPlanet, [PPlanet])
randomPlanets radius = do
  (c, Planet _ s _) <- randomPlanet radius []
  let home1 = (c, Planet (Owned Player1) s (Growth 5))

  pref <- newIORef @[PPlanet] []

  replicateM_ (radius `div` 2) $ do
    ((x, y), p) <- readIORef pref >>= randomPlanet radius
    modifyIORef pref (\ps -> ((x, y), p) : ps)
  ps <- readIORef pref
  return (home1, ps)

type RenderableMap = (Map PlanetId Point, Planets, Wormholes)

randomMap :: Int -> StdGen -> IO RenderableMap
randomMap radius g = do
  old_gen <- getStdGen
  setStdGen g

  -- Create planets for the half of the map for player 1
  ((c, home1), ps) <- randomPlanets radius

  let ps' = sortOn (\(c', _) -> distance c c') ps
      planets1_ :: [(PlanetId, PPlanet)]
      planets1_ = zip (map PlanetId [1..]) ps'

  -- Create paths from the home planet to all other planets
  (wormholes1 :: [Wormhole], planets1 :: [(PlanetId, (Point, Planet))]) <- foldM
    (\(wormholes, connected) target@(target_id, (target_pos, _)) -> do
        let (source_id, (source_pos, _))
              = head (sortOn (\(_, (c', _)) -> distance target_pos c') connected)
            turns  = Turns (round (distance source_pos target_pos))
            wormhole = Wormhole (Source source_id) (Target target_id) turns
        return (wormhole : wormholes, target : connected)
    )
    ([], [(PlanetId 0, (c, home1))]) planets1_

  -- Mirror the planets for player 2 
  let
    home2_id
      = length planets1

    twinId :: PlanetId -> PlanetId
    twinId (PlanetId p)
      = PlanetId ((p+home2_id) `mod` (2*home2_id))

    planets2 :: [(PlanetId, (Point, Planet))]
    planets2
      = mirrorPlanets home2_id planets1

    planets' :: Map PlanetId (Point, Planet)
    planets'
      = M.fromList (planets1 ++ planets2)

    planets_list = M.assocs planets'
    planets_count = length planets_list


  -- Generating some more wormholes for each planet
  (battle_wormholes :: [Wormhole]) <- fmap concat . forM planets1 $ \(pId, (pos, _)) -> do
    roll <- randomRIO @Int (1, 100)
    if roll <= 70
      then do
        index <- randomRIO (0, planets_count - 1)
        let (pId', (pos', _)) = planets_list !! index
            dis = distance pos pos'
        if dis <= 3 then
          return []
        else do
            let turns = Turns (round dis)
            return [ Wormhole (Source pId) (Target pId') turns ]
      else
        return []

  let mirrorWormholes :: [Wormhole] -> [Wormhole]
      mirrorWormholes = map (\(Wormhole (Source x) (Target y) t) -> 
                               Wormhole (Source (twinId x)) (Target (twinId y)) t) 

      dist = round (distance (fst (planets' M.! 1)) (fst (planets' M.! (twinId 0))))

      homeWormhole = Wormhole (Source 1) (Target (twinId 0)) (Turns dist)

      halfWormholes = [homeWormhole] ++ wormholes1 ++ battle_wormholes

      wormholes = halfWormholes ++ mirrorWormholes halfWormholes
      

  -- If a planet has no incoming or outgoing edges, connect it to its twin.
  new_ws <- forM planets1 $ \(pId, (pos, _)) -> do
    let
      pId' = twinId pId
      twinPos = fst (planets' M.! pId')
      turns = Turns (round (distance pos twinPos))
    if isSink pId wormholes || isSource pId wormholes
      then return [ Wormhole (Source pId) (Target pId') turns
                  , Wormhole (Source pId') (Target pId) turns
                  ]
      else return []

  let ws_no_sinks = wormholes ++ concat new_ws

  setStdGen old_gen

  return ( M.map fst planets'
         , M.map snd planets'
         , M.fromList (zip (map WormholeId [0..]) ws_no_sinks))

  where isSink :: PlanetId -> [Wormhole] -> Bool
        isSink pId ws = null (filter ((==pId) . source) ws)

        isSource :: PlanetId -> [Wormhole] -> Bool
        isSource pId ws = null (filter ((==pId) . target) ws)

        source :: Wormhole -> PlanetId
        source (Wormhole (Source s) _ _) = s

        target :: Wormhole -> PlanetId
        target (Wormhole (Source s) _ _) = s

        shiftPlanetId :: Int -> PlanetId -> PlanetId
        shiftPlanetId by (PlanetId pId) = PlanetId (by + pId)

        shiftWormhole :: Int -> Wormhole -> Wormhole
        shiftWormhole by (Wormhole (Source s) (Target t) turns)
          = Wormhole (Source (shiftPlanetId by s)) (Target (shiftPlanetId by t)) turns

        mirrorPlanets :: Int -> [(PlanetId, PPlanet)] -> [(PlanetId, PPlanet)]
        mirrorPlanets by = map (\(pId, planet) -> (shiftPlanetId by pId, mirrorPlanet planet))

        same_wh :: Wormhole -> Wormhole -> Bool
        same_wh (Wormhole (Source s) (Target t) _) (Wormhole (Source s') (Target t') _)
          = s == s' && t == t'

distance :: Point -> Point -> Double
distance (x, y) (x', y')
  = let dx = x - x'
        dy = y - y'
    in sqrt (fromIntegral (dx^2 + dy^2))

--------------------------------------------------------------------------------
-- Render

newtype Render a = Render { runRender :: State Builder a }
  deriving (Functor, Applicative, Monad)

draw :: Handle -> Render a -> IO a
draw h (Render st) = do
  let (a, s) = runState st mempty
  hPutBuilder h s
  return a

append :: String -> Render ()
append str = Render (modify (<> string7 str))

appendLn :: String -> Render ()
appendLn str = Render (modify (\s -> s <> string7 str <> char7 '\n'))

setNeutralCol :: Render ()
setNeutralCol =
  resetCol
  -- append "\x1b[30m"

setP1Col :: Render ()
setP1Col = do
  append "\x1b[44m"
  append "\x1b[97m"

setP2Col :: Render ()
setP2Col = do
  append "\x1b[41m"
  append "\x1b[97m"

resetCol :: Render ()
resetCol =
  append "\x1b[0m"

displayPlanet :: Planet -> Render ()
displayPlanet (Planet o (Ships s) (Growth g)) = do
  let playerCol = case o of
        Neutral       -> setNeutralCol
        Owned Player1 -> setP1Col
        Owned Player2 -> setP2Col
  playerCol
  append "\ESC7"
  append $ "    "
  append "\ESC8"
  append $ "[+" ++ show g ++ "]"
  append "\ESC8"
  append "\ESC[1B"
  append $ "    "
  append "\ESC8"
  append "\ESC[1B"
  append $ show s
  resetCol

drawBG :: Int -> Render ()
drawBG radius = do
  moveCursor (1,1)
  forM_ [- radius.. radius] $ \y -> do
    forM_ [- radius.. radius] $ \x -> do
      if not (inMap radius (x, y))
        then append "."
        else append " "
      append " "
    appendLn ""
  resetCol
  moveCursor (1,1)

mapToScreen :: Int -> Point -> Point
mapToScreen radius (x, y) = ((radius + x)*2 + 1, radius + y + 1)

moveCursor :: Point -> Render ()
moveCursor (x, y) = 
  append $ "\ESC[" ++ show y  ++ ";" ++ show x ++ "H"

drawPlanets :: Int -> RenderableMap -> Render ()
drawPlanets radius (positions, planets, _) = do
  forM_ (M.assocs positions) $ \ (pId, c) -> do
    moveCursor (mapToScreen radius c)
    displayPlanet (fromJust (M.lookup pId planets))

drawMap :: Int -> RenderableMap -> Fleets -> Render ()
drawMap radius rm@(positions, _, wormholes) fleets = do

  forM_ (M.elems wormholes) $ \(Wormhole (Source s) (Target t) _) -> do
    let c_s = mapToScreen radius $ fromJust (M.lookup s positions)
        c_t = mapToScreen radius $ fromJust (M.lookup t positions)
    drawLine c_s c_t

  forM_ fleets $ \(Fleet p _ wId (Turns remaining)) -> do
    let Wormhole (Source s) (Target t) (Turns total) = fromJust (M.lookup wId wormholes)
        c_s = mapToScreen radius $ fromJust (M.lookup s positions)
        c_t = mapToScreen radius $ fromJust (M.lookup t positions)
    drawFleet p c_s c_t (fromIntegral (total - remaining) / fromIntegral total)

  drawPlanets radius rm

  resetCol
  moveCursor (1, radius * 2 + 1)

inMap :: Int -> Point -> Bool
inMap radius (x, y) = x^2 + y^2 <= radius ^2

drawFleet :: Player -> Point -> Point -> Double -> Render ()
drawFleet player p1 p2 progress = do
  let ps = line p1 p2
  setPCol player
  let (x, y) = ps !! (max 0 (floor $ (fromIntegral $ length ps) * progress))
  moveCursor (x, y)
  append " "
  where setPCol Player1 = setP1Col
        setPCol Player2 = setP2Col

drawLine :: Point -> Point -> Render ()
drawLine p1 p2 = do
  append "\ESC[90m"
  let ps = line p1 p2
  forM_ ps $ \(x, y) -> do
    moveCursor (x, y)
    append "."
  resetCol

-- from https://wiki.haskell.org/Bresenham%27s_line_drawing_algorithm (tweaked)
line :: Point -> Point -> [Point]
line pa@(xa,ya) pb@(xb,yb)
  = final . map maySwitch . unfoldr go $ (x1,y1,0)
  where
    steep = abs (yb - ya) > abs (xb - xa)
    maySwitch = if steep then (\(x,y) -> (y,x)) else id
    [(x1,y1),(x2,y2)] = sort [maySwitch pa, maySwitch pb]
    final = if maySwitch pa < maySwitch pb then id else reverse
    deltax = x2 - x1
    deltay = abs (y2 - y1)
    ystep = if y1 < y2 then 1 else -1
    go (xTemp, yTemp, err)
        | xTemp > x2 = Nothing
        | otherwise  = Just ((xTemp, yTemp), (xTemp + 1, newY, newErr))
        where
          tempError = err + deltay
          (newY, newErr) = if (2*tempError) >= deltax
                            then (yTemp+ystep,tempError-deltax)
                            else (yTemp,tempError)

checkMap :: RenderableMap -> Bool
checkMap (ppoints, planets, wormholes) = isSimpleGraph && isSymmetric && noSingleton where
  isSimpleGraph :: Bool
  isSimpleGraph = noDuplicate cmp (edges gs) && all (not . selfLoop) (edges gs)
    where cmp :: (WormholeId, Wormhole) -> (WormholeId, Wormhole) -> Bool
          cmp (_, Wormhole s t _) (_, Wormhole s' t' _) = s == s' && t == t'

  isSymmetric :: Bool
  isSymmetric = even numPlanets && all hasTwinEdge (edges gs) 

  hasTwinEdge :: (WormholeId, Wormhole) -> Bool 
  hasTwinEdge (_, Wormhole (Source s) (Target t) n) = elem (Wormhole (Source (twinId s)) (Target (twinId t)) n) (map snd (edges gs)) 
  

  noSingleton :: Bool
  noSingleton = all (\v -> not (null (edgesFrom gs v)) && not (null (edgesTo gs v))) 
                    (vertices gs)

  gs :: GameState
  gs = GameState planets wormholes []

  noDuplicate :: (a -> a -> Bool) -> [a] -> Bool
  noDuplicate eq []     = True
  noDuplicate eq (x:xs) = all (\x' -> not (eq x x')) xs && noDuplicate eq xs

  selfLoop :: (WormholeId, Wormhole) -> Bool
  selfLoop (_, Wormhole (Source s) (Target t)  _) = (s == t)

  numPlanets :: Int
  numPlanets = M.size planets

  twinId :: PlanetId -> PlanetId
  twinId (PlanetId p)
    = PlanetId ((p + (numPlanets `div` 2)) `mod` numPlanets)

  mirrorWormholes :: [Wormhole] -> [Wormhole]
  mirrorWormholes = map (\(Wormhole (Source x) (Target y) t) -> 
                           Wormhole (Source (twinId x)) (Target (twinId y)) t) 
