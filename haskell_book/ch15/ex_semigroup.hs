import Data.Semigroup
import Test.QuickCheck
import Control.Monad.RWS (Monoid)
import Data.Semigroup (Semigroup)
import GHC.Base (undefined)

data Trivial = Trivial deriving (Eq, Show)

instance Arbitrary Trivial where
    arbitrary = return Trivial

instance Semigroup Trivial where
    _ <> _ = Trivial

instance Monoid Trivial where
    mempty = Trivial

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity x = x <> mempty == x

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity x = mempty <> x == x

type TrivAssoc = Trivial -> Trivial -> Trivial -> Bool


newtype Identity a = Identity a deriving (Eq, Show)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        Identity <$> arbitrary

instance (Semigroup a) => Semigroup (Identity a) where
    (Identity x) <> (Identity y) = Identity (x <> y)

instance (Monoid a) => Monoid (Identity a) where
    mempty = Identity mempty

type IdAssoc = Identity String -> Identity String -> Identity String -> Bool


data Two a b = Two a b deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        x <- arbitrary
        Two x <$> arbitrary

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
    (Two x0 y0) <> (Two x1 y1) = Two (x0 <> x1) (y0 <> y1)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
    mempty = Two mempty mempty

type TwoAssoc = Two String String -> Two String String -> Two String String -> Bool


newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Arbitrary BoolConj where
    arbitrary = frequency [(1, return (BoolConj True)), (1, return (BoolConj False))]

instance Semigroup BoolConj where
    (BoolConj x) <> (BoolConj y) = BoolConj (x && y)

instance Monoid BoolConj where
    mempty = BoolConj True

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool


data Or a b = Fst a | Snd b deriving(Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        elements [Fst x, Snd y]
instance (Semigroup a, Semigroup b) => Semigroup (Or a b) where
    (Snd x) <> _ = Snd x
    (Fst x) <> (Snd y) = Snd y
    (Fst _) <> (Fst y) = Fst y


type OrAssoc = Or Trivial Trivial -> Or Trivial Trivial -> Or Trivial Trivial -> Bool


combineSemigroupAssoc :: 
    (Eq b, Semigroup b) 
    => Combine a b 
    -> Combine a b 
    -> Combine a b 
    -> a 
    -> Bool
combineSemigroupAssoc f g h a = unCombine (f <> (g <> h)) a == unCombine ((f <> g) <> h) a

combineMonoidLeftIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineMonoidLeftIdentity f a = unCombine (f <> mempty) a == unCombine f a

combineMonoidRightIdentity :: (Eq b, Monoid b) => Combine a b -> a -> Bool
combineMonoidRightIdentity f a = unCombine (mempty <> f) a == unCombine f a

newtype Combine a b = Combine { unCombine :: a -> b}
instance Show (Combine a b) where show _ = "Combine"
instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
    arbitrary = Combine <$> arbitrary
instance (Semigroup b) => Semigroup (Combine a b) where
    (Combine f) <> (Combine g) = Combine (f <> g)
instance (Monoid b) => Monoid (Combine a b) where
    mempty = Combine mempty

type CombAssoc a b = Combine a b -> Combine a b -> Combine a b -> a -> Bool



newtype Mem s a = Mem {
    runMem :: s -> (a, s)
}


instance (CoArbitrary s, Arbitrary s, Arbitrary a) => Arbitrary (Mem s a) where
    arbitrary = Mem <$> arbitrary

instance (Semigroup a) => Semigroup (Mem s a) where
    (Mem f) <> (Mem g) = 
        Mem $ \s ->
            let (a', s') = g s
                (a'', s'') = f s'
            in (a'' <> a', s'')

instance (Monoid a) => Monoid (Mem s a) where
    mempty = Mem  $ \s -> (mempty s)

instance Show (Mem s a) where
    show (Mem _) = "Mem"

memSemigroupAssoc :: ( Eq s, Eq a, Semigroup a) =>
    Mem s a -> Mem s a -> Mem s a -> s -> Bool
memSemigroupAssoc f g h x = runMem (f <> (g <> h)) x == runMem ((f <> g) <> h) x

type MemAssoc s a = Mem s a -> Mem s a -> Mem s a -> s -> Bool


testF :: Int -> (String, Int)
testF x = ("hi", x + 1)

f' = Mem testF

main :: IO ()
main = do
    let rmzero = runMem mempty 0
        rmleft = runMem (f' <> mempty ) 0
        rmright = runMem (mempty <> f') 0
    print $ rmleft
    print $ rmright
    print $ (rmzero :: (String, Int))
    print $ rmleft == runMem f' 0
    print $ rmright == runMem f' 0


    quickCheck (semigroupAssoc :: TrivAssoc)
    quickCheck (semigroupAssoc :: IdAssoc)
    quickCheck (semigroupAssoc :: TwoAssoc)
    quickCheck (semigroupAssoc :: BoolConjAssoc)
    quickCheck (semigroupAssoc :: OrAssoc)
    quickCheck (combineSemigroupAssoc :: CombAssoc Float (Product Int))
    -- quickCheck (memSemigroupAssoc :: MemAssoc Int String)
    
    let mli = monoidLeftIdentity
        mri = monoidRightIdentity

    quickCheck (mli :: Trivial -> Bool)
    quickCheck (mri :: Trivial -> Bool)

    quickCheck (monoidLeftIdentity :: Identity String -> Bool)
    quickCheck (monoidRightIdentity :: Identity String -> Bool)

    quickCheck (monoidLeftIdentity :: Two String Trivial -> Bool)
    quickCheck (monoidRightIdentity :: Two String Trivial -> Bool)

    quickCheck (monoidLeftIdentity :: BoolConj -> Bool)
    quickCheck (monoidRightIdentity :: BoolConj -> Bool)

    quickCheck (combineMonoidLeftIdentity :: Combine Float (Sum Int) -> Float -> Bool)
    quickCheck (combineMonoidRightIdentity :: Combine Float (Sum Int) -> Float -> Bool)

{-

fmap 

-}