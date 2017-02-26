import Data.Vect

readSomeWords : IO (len ** Vect len String)
readSomeWords =
  do s <- getLine
     case trim s of
       "" => pure (0 ** [])
       x => do (lenMinus1 ** xs) <- readSomeWords
               pure (S lenMinus1 ** x :: xs)

readAllWords : (len : Nat) -> IO (Vect len String)
readAllWords Z = pure []
readAllWords (S k) = do s <- getLine
                        case trim s of
                          "" => do putStrLn $ "Please input " ++ (show (S k)) ++ " more words."
                                   readAllWords (S k)
                          x => do xs <- readAllWords k
                                  pure (x :: xs)

main : IO ()
main = do putStrLn "Input first names, leaving a blank line to finish"
          (n ** firstNames) <- readSomeWords
          putStrLn ("Now input " ++ (show n) ++ " last names")
          lastNames <- readAllWords n
          putStrLn ("Your full names are: ")
          let fullNames = zip firstNames lastNames
          putStrLn (show fullNames)
