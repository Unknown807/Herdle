import Base
import PartA
import PartB
import PartC

-- iteration 1

loop :: String -> Int -> IO ()
loop _ 0 = putStrLn (prompt Lose) >> return ()
loop word n = do
    userGuess <- getGuess (length word) (['a'..'z']++['A'..'Z'])
    guessStats <- return [s | (c, s) <-(checkGuess userGuess word)]
    
    statuses <- return (showStatus guessStats)
    putStrLn statuses

    if (and $ map (== Here) guessStats) then do
        putStrLn (prompt Win)
        return ()
    else
        loop word (n-1)