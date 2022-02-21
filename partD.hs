import Base
import PartA
import PartB
import PartC

-- iteration 1

loop :: String -> Int -> IO ()
loop _ 0 = putStrLn (prompt Lose) >> return ()
loop word n = do
    userGuess <- getGuess (length word) ['a'..'z']++['A'..'Z']
    guessStatus <- checkGuess userGuess word
    showStatus guessStatus
    if (1 == 2) then
        putStrLn (prompt Win) >> return ()
    else
        loop word (n-1)