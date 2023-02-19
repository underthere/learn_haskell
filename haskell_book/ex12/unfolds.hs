
myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : myIterate f (f x)

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f x = inner f (f x)
  where
  inner :: (b -> Maybe(a, b)) -> Maybe (a, b) -> [a]
  inner f Nothing = []
  inner f (Just(c, n)) = c : inner f (f n)


betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr f'
    where f' x = Just (x, f x)


data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)


unfoldTree :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfoldTree f x = inner f (f x)
    where
    inner :: (a -> Maybe (a,b,a)) -> Maybe (a,b,a) -> BinaryTree b
    inner f' (Just(l,v,r)) = Node (inner f' (f' l)) v (inner f' (f' r))
    inner f' Nothing = Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = unfoldTree f' 0
    where
    f' x = if (==) x n then Nothing else Just (x + 1, x, x + 1)
