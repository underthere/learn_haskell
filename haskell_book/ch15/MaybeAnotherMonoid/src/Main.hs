module Main (main) where

import Data.Monoid
import Test.QuickCheck

monoidAssoc :: (Eq m, Monoid m)
              => m -> m -> m -> Bool
monoidAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)


monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

data Optional a = Nada
                | Only a
                deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Optional a) where
  (<>) (Only a) (Only b) = Only ( a <> b)
  (<>) Nada (Only b) = Only b
  (<>) (Only a) Nada = Only a
  (<>) Nada Nada = Nada

instance (Monoid a) => Monoid (Optional a) where
  mempty                    = Nada


newtype First' a = First' { getFirst' :: Optional a} deriving (Eq, Show)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    x <- arbitrary
    frequency [(1, return $ First' $ Only x),
               (1, return $ First' Nada)
              ]

instance Semigroup (First' a) where
  (<>) (First' (Only x)) _ = First' (Only x)
  (<>) (First' Nada) (First' (Only x)) = First' (Only x)
  (<>) (First' Nada) (First' Nada) = First' Nada

instance Monoid (First' a) where
  mempty = First' Nada

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

type FirstMappend = First' String -> First' String -> First' String -> Bool
type FstId = First' String -> Bool

main :: IO ()
main = do
  quickCheck (monoidAssoc :: FirstMappend)
  quickCheck (monoidLeftIdentity :: FstId)
  quickCheck (monoidRightIdentity :: FstId)
