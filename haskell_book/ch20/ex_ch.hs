import Distribution.Simple.Install (install)

data Constant' a b = Constant' b

instance Foldable (Constant' a) where
  foldr f z (Constant' x) = f x z

data Two a b = Two a b

instance Foldable (Two a) where
  foldr f z (Two _ x) = f x z

data Three a b c = Three a b c

instance Foldable (Three a b) where
  foldr f z (Three _ _ x) = f x z

data Three' a b = Three' a b b

instance Foldable (Three' a) where
  foldr f z (Three' _ x y) = f x (f y z)

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
  foldr f z (Four' _ x y z') = f x (f y (f z' z))

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF f = foldMap (\x -> if f x then pure x else mempty)

