
import Base

-- iteration 1

checkGuess :: String -> String -> [(Char, Status)]
checkGuess guess word = checkGuess' guess word 0

checkGuess' :: String -> String -> Int -> [(Char, Status)]
checkGuess' [] word _ = []
checkGuess' (x:xs) word cnt
    | ( x == (word!!cnt) ) = (x, Here) : checkGuess' xs word (cnt+1)
    | otherwise = (x, Nowhere) : checkGuess' xs word (cnt+1)

-- iteration 2

