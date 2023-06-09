import Employee
import Data.Tree

-- Exercise 1

-- | glCons
glCons :: Employee -> GuestList -> GuestList
glCons e@(Emp {empFun = fun}) (GL lst totFun) = GL (e:lst) (fun + totFun)


instance Semigroup GuestList where
    (GL ll lf) <> (GL rl rf) = GL (ll <> rl) (lf + rf)

instance Monoid GuestList where
    mempty = GL [] 0
    
moreFun :: GuestList -> GuestList -> GuestList
moreFun = max


treeFold :: (a -> a -> a) -> a -> Tree a -> a
treeFold f e (Node {rootLabel = x, subForest=[]}) = x
treeFold f e (Node {rootLabel = x, subForest = childs}) = f x (foldl f e (map (treeFold f e) childs))

