
import Base
import Data.Char

-- iteration 1

getGuess :: Int -> String -> IO String
getGuess n availableChars = getGuess' n 0 "" availableChars

getGuess' :: Int -> Int -> String -> String -> IO String
getGuess' n m cword availableChars 
    | n > 0 = do
        inp <- getChar'
        inp <- if (inp `elem` availableChars) then
                getGuess' (n-1) (m+1) (cword++(toLower inp):"") availableChars
            else do
                if (inp == '.' && m /= 0) then do
                    putStr "\b\b"
                    getGuess' (n+1) (m-1) ( take (m-1) cword ) availableChars
                else do
                    putChar '\b'
                    getGuess' n m cword availableChars
        return inp

    | otherwise = do 
        putChar '\n'
        return cword

