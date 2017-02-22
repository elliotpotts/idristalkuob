import Data.Vect

readVect : IO (len ** Vect len String)
readVect = do s <- getLine
              case trim s of
                "" => pure (_ ** [])
                x => do (_ ** xs) <- readVect
                        pure (_ ** x :: xs)

putFullNames : Vect k (String, String) -> IO ()
putFullNames [] = pure ()
putFullNames (x :: xs) = do putStrLn (show x)
                            putFullNames xs

main : IO ()
main = do putStrLn "Enter first names"
          (len_a ** first_names) <- readVect
          putStrLn "Enter the last names"
          (len_b ** last_names) <- readVect
          (case decEq len_a len_b of
                (Yes Refl) => putFullNames (zip first_names last_names)
                (No contra) => do putStrLn "Please enter the same number of first names and last names"
                                  main)
