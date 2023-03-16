module Main (main) where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes


data List a = 
              Nill
            | Cons a (List a)
            deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nill), (3, return (Cons a Nill))]

instance Semigroup (List a) where
  Nill <> ys = ys
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Functor List where
  fmap _ Nill = Nill
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
  pure x = Cons x Nill
  Nill <*> _ = Nill
  _ <*> Nill = Nill
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)


instance Eq a => EqProp (List a) where
  (=-=) = eq

listTrigger :: List (String, Int, Maybe Int)
listTrigger = undefined

----------------------------------------------

take' :: Int -> List a -> List a
take' 0 _ =  Nill
take' n xs = case xs of
  Nill -> Nill
  Cons x xs' -> Cons x (take' (n - 1) xs')

newtype ZipList' a = ZipList' (List a)
  deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
  xs =-= ys = xs' `eq` ys'
    where xs' = let (ZipList' l) = xs
                in take' 3000 l
          ys' = let (ZipList' l) = ys
                in take' 3000 l

instance Functor ZipList' where
  fmap f (ZipList' xs) = ZipList' (fmap f xs)

instance Applicative ZipList' where
  pure = ZipList' . pure
  ZipList' fs <*> ZipList' xs = ZipList' (fs <*> xs)

instance Arbitrary a => Arbitrary (ZipList' a) where
  arbitrary = ZipList' <$> arbitrary

ziplistTrigger :: ZipList' (String, Int, Maybe Int)
ziplistTrigger = undefined


---- Variations on Either ----
data Validation e a = 
    Failure' e
  | Success' a
  deriving (Eq, Show)


instance Functor (Validation e) where
  fmap _ (Failure' e) = Failure' e
  fmap f (Success' a) = Success' (f a)

instance (Monoid e) => Applicative (Validation e) where
  pure = Success'
  Failure' e <*> Failure' e' = Failure' (e <> e')
  Failure' e <*> Success' _ = Failure' e
  Success' _ <*> Failure' e = Failure' e
  Success' f <*> Success' a = Success' (f a)

instance (Arbitrary e, Arbitrary a) => Arbitrary (Validation e a) where
  arbitrary = do
    e <- arbitrary
    a <- arbitrary
    frequency [(1, return (Failure' e)), (1, return (Success' a))]

instance (Eq e, Eq a) => EqProp (Validation e a) where
  (=-=) = eq

validationTrigger :: Validation String (String, Int, Maybe Int)
validationTrigger = undefined

main :: IO ()
main = do
  quickBatch (applicative listTrigger)
  quickBatch $ applicative ziplistTrigger 
  quickBatch $ applicative validationTrigger