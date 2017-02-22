import Data.Vect

vectsum : Vect k Integer -> Integer
vectsum [] = 0
vectsum (x :: xs) = x + (vectsum xs)

mean : Vect (S k) Integer -> Integer
mean xs = (vectsum xs) `div` (toIntegerNat (length xs))

main : IO ()
main = do putStrLn (show (mean [1,2,3]))
          putStrLn (show (mean [1,1,1]))
          putStrLn (show (mean [3]))
