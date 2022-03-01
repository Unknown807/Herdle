import Base
import PartA
import PartB
import PartC
import Data.Char

-- iteration 1, 2 & 3

go :: String -> IO()
go word =  loop (map toLower word) ['a'..'z'] 6

loop :: String -> [Char] -> Int -> IO ()
loop _ _ 0 = putStrLn (prompt Lose) >> return ()
loop word availableChars n = do
    userGuess <- getGuess (length word) availableChars
    checkedGuess <- return (checkGuess userGuess word)
    guessStats <- return [s | (c, s) <- checkedGuess]
    statuses <- return (showStatus guessStats)
    putStrLn statuses

    if (and $ map (== Here) guessStats) then do
        putStrLn (prompt Win)
        return ()
    else
        loop word (updateAvailable availableChars checkedGuess) (n-1)

