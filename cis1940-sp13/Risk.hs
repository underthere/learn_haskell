{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.Functor.Identity (Identity (Identity))

------------------------------------------------------------
-- Die values

newtype DieValue = DV {unDV :: Int}
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

instance Show (Rand StdGen DieValue) where
  show :: Rand StdGen DieValue -> String
  show s = "|Random|"

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield {attackers :: Army, defenders :: Army} deriving (Show)

maxAttackers :: Int
maxAttackers = 3

maxDefenders :: Int
maxDefenders = 2

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x : xs) = leftPart ++ [x] ++ rightPart
  where
    leftPart = qsort [n | n <- xs, n <= x]
    rightPart = qsort [n | n <- xs, n > x]

tuppleAdd :: (Int, Int) -> (Int, Int) -> (Int, Int)
tuppleAdd (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

rollN :: Int -> Rand StdGen [DieValue]
rollN n = replicateM n die

testBF :: Battlefield
testBF = Battlefield {attackers=5, defenders=4}

battle :: Battlefield -> Rand StdGen Battlefield
battle bf@(Battlefield {attackers = survivalAttackers, defenders = survivalDefedeners}) =
  liftM2 applyReulst fightResult (return bf)
  where
    applyReulst (deadAttackers, deadDefenders) (Battlefield {attackers = as, defenders = ds}) = Battlefield {attackers = survivalAttackers - deadAttackers, defenders = survivalDefedeners - deadDefenders}
    fightResult = liftM2 fight attackDices defendDices

    attackDices = reverse . qsort <$> rollN  (bestEffortAttack survivalAttackers)
    defendDices = reverse . qsort <$> rollN  (bestEffortDefend survivalDefedeners)

    bestEffortAttack :: Int -> Int
    bestEffortAttack n
      | n > maxAttackers = maxAttackers
      | otherwise = max 0 (n - 1)

    bestEffortDefend :: Int -> Int
    bestEffortDefend = min maxDefenders

    fight :: [DieValue] -> [DieValue] -> (Army, Army)
    fight [] _ = (0, 0)
    fight _ [] = (0, 0)
    fight (x : xs) (y : ys)
      | x > y = (0, 1) `tuppleAdd` fight xs ys
      | otherwise = (1, 0) `tuppleAdd` fight xs ys

invade :: Battlefield -> Rand StdGen Battlefield
invade bf
  | attackers bf < 2 || defenders bf <= 0 = return bf
  | otherwise = battle bf >>= invade


successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= calcProb
  where
    calcProb bfs = return $ fromIntegral (length x) / fromIntegral (length bfs)
      where x = filter ((==0) . defenders) bfs

extractSuccessProb :: Battlefield -> Double
extractSuccessProb = undefined

