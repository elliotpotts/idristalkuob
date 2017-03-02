import Data.Vect

readSomeWords : IO (len ** Vect len String)
readSomeWords =
  do s <- getLine
     case trim s of
       "" => pure (0 ** [])
       x => do (predLen ** xs) <- readSomeWords
               pure (S predLen ** x :: xs)

readAllWords : (len : Nat) -> IO (Vect len String)
readAllWords Z = pure []
readAllWords (S k) = do s <- getLine
                        case trim s of
                          "" => do putStrLn $ "Please input " ++ (show (S k)) ++ " more words."
                                   readAllWords (S k)
                          x => do xs <- readAllWords k
                                  pure (x :: xs)

joinWithSpace : String -> String -> String
joinWithSpace a b = a ++ " " ++ b

main : IO ()
main = do putStrLn "Input first names, leaving a blank line to finish"
          (n ** firstNames) <- readSomeWords
          putStrLn ("Now input " ++ (show n) ++ " last names")
          lastNames <- readAllWords n
          putStrLn ("Your full names are: ")
          let fullNames = zipWith joinWithSpace firstNames lastNames
          putStrLn (show fullNames)
