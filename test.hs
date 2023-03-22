
data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f (Second b) = Second (f b)
  fmap _ (First a) = First a

instance Applicative (Sum a) where
    pure = Second
    (First a) <*> _ = First a
    (Second f) <*> something = fmap f something

instance Monad (Sum a) where
  return = pure
  (First a) >>= _ = First a
  (Second b) >>= f = f b

twiceEven :: [Integer] -> [Integer]
twiceEven xs = do
  x <- xs
  if even x
    then [x*x, x*x]
    else []
