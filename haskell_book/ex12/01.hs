import qualified Data.Maybe
import Data.Maybe (catMaybes)
splitBy:: Char -> String -> [String]
splitBy _ "" = [];
splitBy del s = foldr f [""] s
    where f :: Char -> [String] -> [String]
          f curChar strings@(part:collected)
            | curChar == del = "":strings
            | otherwise = (curChar:part):collected


joinTwoWith :: Char -> String -> String -> String
joinTwoWith c s0 [] = s0
joinTwoWith c [] s1 = s1
joinTwoWith c [x] s1 = x:c:s1
joinTwoWith c (x:xs) s1 = x: joinTwoWith c xs s1

joinWith :: Char -> [String] -> String
joinWith c ss = foldr (joinTwoWith c) "" ss

notThe :: String -> Maybe String
notThe "the" = Nothing
notThe x = Just x


replaceThe :: String -> String
replaceThe s = joinWith ' ' $ map f0 xx
  where
    xx = map notThe $ splitBy ' ' s
    f0 = Data.Maybe.fromMaybe "a"


wordsToMaybes :: String -> [Maybe String]
wordsToMaybes s = map notThe $ words s

coutTheBeforeVowel :: String -> Integer
coutTheBeforeVowel s = calIt (wordsToMaybes s)
  where
    calIt [] = 0
    calIt [_] = 0
    calIt (Just _ : _ : xs) = calIt xs
    calIt (Nothing: Just x: xs) = if head x `elem` "aeiou" then 1 + calIt xs else calIt xs

isVowels :: Char -> Bool
isVowels c = c `elem` "aeiou"


coutVowels :: String -> Int
coutVowels s = length (filter isVowels s)


coutVowels' :: String -> Integer
coutVowels' = foldl addOneIfVowels 0
  where
    addOneIfVowels acc x = if isVowels x then acc + 1 else acc


newtype Word' = Word' String
  deriving (Eq, Show)

vowelConsonantCounter :: (Int, Int) -> Char -> (Int, Int)
vowelConsonantCounter (v, c) x = if isVowels x then (v + 1, c) else (v, c + 1)


mkWord :: String -> Maybe Word'
mkWord xs
  | uncurry (<=) sums = Just (Word' xs)
  | otherwise = Nothing
  where
    sums = foldl vowelConsonantCounter (0, 0) xs



data Nat = Zero | Succ Nat deriving (Eq, Show)

natToInteger :: Nat -> Integer
natToInteger Zero = 0
natToInteger (Succ x) = 1 + natToInteger x

integerToNat :: Integer -> Maybe Nat
integerToNat x
  | x < 0 = Nothing
  | otherwise = Just $ f x
  where
    f 0 = Zero
    f n = Succ $ f $ n -1


isJust :: Maybe a -> Bool
isJust Nothing = True
isJust _ = False

isNothing :: Maybe a -> Bool
isNothing x = not $ isJust x


maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' b f a = case a of
  Just x -> f x
  Nothing -> b


fromMaybe' :: a -> Maybe a -> a
fromMaybe' a m = case m of
  Just x -> x
  Nothing -> a


listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe (x:xs) = Just x


maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []


catMaybes' :: [Maybe a] -> [a]
catMaybes' [] = []
catMaybes' ((Just x):xs) = x : catMaybes' xs
catMaybes' (Nothing:xs) = catMaybes' xs


flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe = foldr f (Just [])
  where
    f Nothing _ = Nothing
    f _ Nothing = Nothing
    f (Just x) (Just xs) = Just (x : xs)


lefts' :: [Either a b] -> [a]
lefts' = foldr f []
  where
    f (Left x) xs = x:xs
    f (Right _) xs = xs

rights' :: [Either a b] -> [b]
rights' = foldr f []
  where
    f (Left _) xs = xs
    f (Right x) xs = x:xs

partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' = foldr f ([], [])
  where
    f (Left l) (ls, rs) = (l:ls, rs)
    f (Right r) (ls, rs) = (ls, r:rs)


eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Right x) = Just (f x)
eitherMaybe' _ _ = Nothing


either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' lf _ (Left x) = lf x
either' _ rf (Right x) = rf x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' lf rf
  where
    lf _ = Nothing
    rf x = Just $ f x
