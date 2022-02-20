
import Base
import Data.Char

-- iteration 1

getGuess :: Int -> String -> IO String
getGuess n availableChars = getGuess' n "" availableChars

getGuess' :: Int -> String -> String -> IO String
getGuess' n cword availableChars
    | n > 0 = do
        inp <- getChar'
        inp <- if (inp `elem` availableChars) then
                getGuess' (n-1) (cword++(toLower inp):"") availableChars
            else do
                putChar '\b'
                getGuess' n cword availableChars
        return inp

    | otherwise = do 
        putChar '\n'
        return cword

