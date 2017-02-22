import Data.Vect

zipvect : (a -> b -> c) -> Vect k a -> Vect k b -> Vect k c
zipvect f [] [] = []
zipvect f (x :: xs) (y :: ys) = f x y :: zipvect f xs ys
