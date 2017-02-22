import Data.Vect

main : IO ()
main = do let xs : Vect 4 Integer  = [1,2,3,4]
          let x = index 3 xs
          pure ()
