module Main (main) where

import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

----
data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
  fmap _ NopeDotJpg = NopeDotJpg

instance Applicative Nope where
  pure _ = NopeDotJpg
  _ <*> _ = NopeDotJpg

instance Monad Nope where
  return = pure
  _ >>= _ = NopeDotJpg

instance Arbitrary a => Arbitrary (Nope a) where
  arbitrary = return NopeDotJpg

instance Eq a => EqProp (Nope a) where
  (=-=) = eq

nopeTrigger :: Nope (String, Integer, [String])
nopeTrigger = undefined

----

data PbtEither b a = PbtLeft a | PbtRight b
  deriving (Eq, Show)

instance Functor (PbtEither b) where
  fmap f (PbtLeft a) = PbtLeft (f a)
  fmap _ (PbtRight b) = PbtRight b

instance (Monoid b) => Applicative (PbtEither b) where
  pure = PbtLeft
  (PbtRight b) <*> (PbtRight b') = PbtRight b
  (PbtRight b) <*> (PbtLeft _) = PbtRight b
  (PbtLeft _) <*> (PbtRight b) = PbtRight b
  (PbtLeft f) <*> (PbtLeft a) = PbtLeft (f a)

instance (Monoid b) =>  Monad (PbtEither b) where
  return = pure
  (PbtRight b) >>= _ = PbtRight b
  (PbtLeft a) >>= f = f a

instance (Arbitrary a, Arbitrary b) => Arbitrary (PbtEither b a) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    frequency [(1, return (PbtLeft a)), (1, return (PbtRight b))]

instance (Eq a, Eq b) => EqProp (PbtEither b a) where
  (=-=) = eq


pbtEitherTrigger :: PbtEither String (String, Integer, [String])
pbtEitherTrigger = undefined  

---

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
  pure = Identity
  (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = do
    a <- arbitrary
    return (Identity a)

instance Eq a => EqProp (Identity a) where
  (=-=) = eq


identityTrigger :: Identity (String, Integer, Maybe String)
identityTrigger = undefined

---

data List a = Nil | Cons a (List a) deriving (Eq, Show)

instance Functor List where
  fmap _ Nil = Nil
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Semigroup (List a) where
  Nil <> ys = ys
  (Cons x xs) <> ys = Cons x (xs <> ys)

instance Monoid (List a) where
  mempty = Nil
  mappend = (<>)

instance Applicative List where
  pure x = Cons x Nil
  Nil <*> _ = Nil
  _ <*> Nil = Nil
  Cons f fs <*> xs = (f <$> xs) <> (fs <*> xs)

instance Monad List where
  return = pure
  Nil >>= _ = Nil
  Cons x xs >>= f = f x <> (xs >>= f)

instance Arbitrary a => Arbitrary (List a) where
  arbitrary = do
    a <- arbitrary
    frequency [(1, return Nil), (3, return (Cons a Nil))]

instance Eq a => EqProp (List a) where
  (=-=) = eq

listTrigger :: List (String, Integer, Maybe String)
listTrigger = undefined

main :: IO ()
main = do
  quickBatch $ monad nopeTrigger
  quickBatch $ monad pbtEitherTrigger  
  quickBatch $ monad identityTrigger
  quickBatch $ monad listTrigger