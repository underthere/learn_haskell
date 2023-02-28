import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) =>
                    f a
                -> Fun a b
                -> Fun b c
                -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a deriving(Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = Identity <$> arbitrary


data Pair a = Pair a a deriving(Eq, Show)

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)
instance Arbitrary a => Arbitrary (Pair a) where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Pair x y


data Two a b = Two a b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        return $ Two a b
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)


data Three a b c = Three a b c deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (Three a b c) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        return $ Three a b c
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)


data Three' a b = Three' a b b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Three' a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        b' <- arbitrary
        return $ Three' a b b'
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)


data Four a b c d = Four a b c d deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b, Arbitrary c, Arbitrary d) => Arbitrary (Four a b c d) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        c <- arbitrary
        d <- arbitrary
        return $ Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)


data Four' a b = Four' a a a b deriving (Eq, Show)
instance (Arbitrary a, Arbitrary b) => Arbitrary (Four' a b) where
    arbitrary = do
        a <- arbitrary
        a' <- arbitrary
        a'' <- arbitrary
        b <- arbitrary
        return $ Four' a a' a'' b
instance Functor (Four' a) where
    fmap f (Four' x y z b) = Four' x y z (f b)


-- -- -- -- -- -- -- --


type IdIdentity f a = f a -> Bool
type CompIdentity f a b c = f a -> Fun a b -> Fun b c -> Bool

type IdPair f a = f a -> Bool
type CompPair f a b c = f a -> Fun a b -> Fun b c -> Bool

type IdTwo f x a = f x a -> Bool
type CompTwo f x a b c = f x a -> Fun a b -> Fun b c -> Bool


type IdThree f x y a = f x y a -> Bool
type CompThree f x y a b c = f x y a -> Fun a b -> Fun b c -> Bool


type IdFour f x y z a = f x y z a -> Bool
type CompFour f x y z a b c = f x y z a -> Fun a b -> Fun b c -> Bool



main :: IO ()
main = do
    quickCheck (functorIdentity :: IdIdentity Identity String)
    quickCheck (functorCompose' :: CompIdentity Identity Float String Int)
    
    quickCheck (functorIdentity :: IdPair Pair String)
    quickCheck (functorCompose' :: CompPair Pair String Float String)

    quickCheck (functorIdentity :: IdTwo Two String Float)
    quickCheck (functorCompose' :: CompTwo Two Char Float String Int)

    quickCheck (functorIdentity :: IdThree Three String Float Int)
    quickCheck (functorCompose' :: CompThree Three String Float Int Float String)

    quickCheck (functorIdentity :: IdTwo Three' String Float)
    quickCheck (functorCompose' :: CompTwo Three' Char Float String Int)

    quickCheck (functorIdentity :: IdFour Four String String Float Int)
    quickCheck (functorCompose' :: CompFour Four Char String Int Float Char String)

    quickCheck (functorIdentity :: IdTwo Four' String Float)
    quickCheck (functorCompose' :: CompTwo Four' Char Float String Int)