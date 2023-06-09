data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Eq)

-- instance Foldable Tree where

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node x l r) = f (treeFold e f l) x (treeFold e f r)

