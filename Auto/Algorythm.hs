{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
module Auto.Algorythm where

import Prelude
import Data.List (foldl')

mean :: forall a (t :: * -> *). (Fractional a, Foldable t) => t Int -> a
mean a = fromA $ foldl' add (A 0 0) a

data A = A !Int !Int

add :: A -> Int -> A
add (A a b) c = A (a + c) (b + 1)

fromA :: forall a. Fractional a => A -> a
fromA (A a b) = fromIntegral a / (fromIntegral b)

data Score = OutLeft | Minus20 | Minus10 | Equal | Plus10 | Plus20 | Plus30 | OutRight

instance Show Score where
  show OutLeft  = "Крайнее левое"
  show Minus20  = "-20%"
  show Minus10  = "-10%"
  show Equal    = "Равные"
  show Plus10   = "+10%"
  show Plus20   = "+20%"
  show Plus30   = "+30%"
  show OutRight = "Крайне правое"

determineScore :: Int -> Double -> Score
determineScore inp calc
  | fromIntegral inp == calc = Equal
  | fromIntegral inp <  calc = compareLeft (fromIntegral inp) calc
  | otherwise                = compareRight (fromIntegral inp) calc

compareLeft :: Double -> Double -> Score
compareLeft inp calc
  | (1.1 * inp <= calc) && (1.2 * inp > calc) = Minus20
  | 1.2 * inp <= calc = OutLeft
  | otherwise = Minus10

compareRight :: Double -> Double -> Score
compareRight inp calc
  | inp <= 1.1 * calc = Plus10
  | (inp <= 1.2 * calc) && (inp > 1.1 * calc) = Plus20
  | (inp <= 1.3 * calc) && (inp > 1.2 * calc) = Plus30
  | otherwise = OutRight
