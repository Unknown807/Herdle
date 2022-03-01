import Base
import PartA
import PartB
import PartC
import Data.Char

-- iteration 1, 2 & 3

go :: String -> IO()
go word =  loop (map toLower word) ['a'..'z'] 1 6

loop :: String -> [Char] -> Int -> Int -> IO ()
loop _ _ _ 0 = putStrLn (prompt Lose) >> return ()
loop word availableChars attemptN loopN = do
    putStrLn ("Attempt "++(show attemptN))
    putStr (prompt Start)

    userChoice <- getChar'
    if (userChoice == 'q') then do
        putStrLn ('\n' : prompt Quit)
        return ()
    else do
        putStr ('\n' : prompt Guess)
        userGuess <- getGuess (length word) availableChars
        checkedGuess <- return (checkGuess userGuess word)
        guessStats <- return [s | (c, s) <- checkedGuess]
        statuses <- return (showStatus guessStats)
        
        putStrLn (leftMargin++statuses)

        if (and $ map (== Here) guessStats) then do
            putStrLn (prompt Win)
            return ()
        else
            loop word (updateAvailable availableChars checkedGuess) (attemptN+1) (loopN-1)

