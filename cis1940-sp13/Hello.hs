
main :: IO ()
main = do
    getLine >>= (\x -> putStrLn ("Hello, World! " ++ x))