listsum : List Integer -> Integer
listsum [] = 0
listsum (x :: xs) = x + (listsum xs)

mean : List Integer -> Integer
mean xs = (listsum xs) `div` (toIntegerNat (length xs))

main : IO ()
main = do putStrLn (show (mean [1,2,3]))
          putStrLn (show (mean [1,1,1]))
          putStrLn (show (mean []))
