import Employee
import Data.Tree
import Data.List (intercalate)

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

-- Exercise 2

treeFold :: (a -> [b] -> b) -> b -> Tree a -> b
treeFold f e (Node {rootLabel = x, subForest = childs}) = f x (map (treeFold f e) childs)

-- Exercise 3
nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss@(Emp {empName=name, empFun=fun}) [] = (GL [boss] fun, mempty)
nextLevel boss@(Emp {empName=name, empFun=fun}) gs = (maximumSafe withBoss, maximumSafe withoutBoss)
    where
        maximumSafe :: (Monoid a, Ord a) => [a] -> a
        maximumSafe [] = mempty
        maximumSafe l = maximum l

        withBoss = map (glCons boss) withoutSubBoss
        withoutBoss = map fst gs
        withoutSubBoss = map snd gs

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry max . treeFold nextLevel (mempty, mempty)

-- Exercise 5

formatGL :: GuestList -> String
formatGL (GL lst fun) = "Total fun: " ++ show fun ++ "\n" ++ intercalate "\n" (map (\(Emp {empName=name}) -> name) lst)

main :: IO ()
main = do
    file_contents <- readFile "company.txt"
    let tree = read file_contents :: Tree Employee
    let guests = maxFun tree
    let getEmps (GL lst _) = lst
    putStrLn (formatGL guests)
    putStrLn (formatRelations (relations tree (getEmps guests)))


relations :: Tree Employee -> [Employee] -> [(Employee, Employee, Bool)]
relations t xs = [(a, b, False) | a <- xs,
                                         b <- xs,
                                         a /= b
                ]


formatRelations :: [(Employee, Employee, Bool)] -> String
formatRelations lst = intercalate "\n" (map fr lst)
    where
        fr (Emp{empName=e0}, Emp{empName=e1}, b) = e0 ++ " is " ++ e1 ++ "'s Boss? " ++ show b


isBossOf :: Tree Employee -> Employee -> Employee -> Bool
isBossOf root a@(Emp {empName=nameOfBoss, empFun=funOfBoss}) b = b `elem` subordinates atree
    where
        subordinates (Node {subForest=s}) = map (\(Node {rootLabel = e}) -> e) s
        atree = findSubTree a root
        findSubTree ::  Employee -> Tree Employee -> Tree Employee
        findSubTree e rt@Node{rootLabel=re, subForest=childs}
            | re == e = rt
            | otherwise = head (map (findSubTree e) childs)
