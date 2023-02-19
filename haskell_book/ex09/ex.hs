import Data.Foldable (maximumBy)

myAnd :: [Bool] -> Bool
myAnd = foldr (&&) True


myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny f = myOr . map f 

myElem :: Eq a => a -> [a] -> Bool
myElem t axs@(x:xs) 
    | null xs = False
    | t == x = True
    | otherwise = myElem t xs

myElem' :: Eq a => a -> [a] -> Bool
myElem' t = myAny ( t == )

myReverse :: [a] -> [a]
myReverse (x:xs) = myReverse xs ++ [x]


myReverse' :: [a] -> [a]
myReverse' l = rev l []
    where
        rev [] a = a
        rev (x:xs) a = rev xs (x:a)

myReverse'' :: [a] -> [a]
myReverse'' = foldl (flip (:)) []

squish :: [[a]] -> [a]
squish = foldr (++) []


squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f = squish . map f

squishAgain :: [[a]] -> [a]
squishAgain = squishMap id

myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr applyCons []
    where applyCons e mapped = f e : mapped

myMaxiumBy :: (a -> a -> Ordering)
           -> [a]
           -> a
myMaxiumBy f (x:xs) = foldl myCons x xs
    where myCons v0 v1 = case f v0 v1 of GT -> v0
                                         _  -> v1
