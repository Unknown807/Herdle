-- Originally part of Base.hs by Dominic Orchard

{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

-- Imports shared by all parts
import System.IO
import Data.List
import Data.Time.Clock
import Data.Time.Calendar
-- Following imports needed for the `getChar'` fix
import Data.Char
import Foreign.C.Types

-- Describe the status of each letter in the guess
data Status =
    Here                         -- Letter in correct place
  | Elsewhere (Maybe Direction)  -- Letter elsewhere in word, with optional direction
  | Nowhere                      -- Letter is not in the word
    deriving (Show, Eq)

-- Describe the position of a letter in a word
-- `Before` represents before and/or after
-- `After` represents strictly after
data Direction = Before | After
    deriving (Show, Eq)

-- Names of various prompts
data Prompt = Guess | Start | Quit | Lose | Win
    deriving (Show, Eq, Enum)

-- Give the string associated with the prompt
prompt :: Prompt -> String
prompt Lose  = "Run out of guesses!"
prompt Win   = "You got it. Well done!"
prompt Start = "Guess [any] or quit [q]? "
prompt Guess = "Ok. Enter your guess:    "
prompt Quit  = "Bye!"

-- Multi-platform version of `getChar` which has a fix for a GHC bug with Windows cmd/Powershell
getChar' :: IO Char
getChar' = do
#ifdef mingw32_HOST_OS
      -- Windows has to do things...
      c <- c_getch
      let c' = chr . fromEnum $ c
      putChar c'
      return c'
#else
    -- Linux, Unix, Mac OS X can just use the normal getChar
    getChar
#endif

#ifdef mingw32_HOST_OS  
foreign import ccall unsafe "conio.h getch"
  c_getch :: IO CInt
#endif

-- Part A, Iteration 1

showStatus :: [Status] -> String
showStatus [] = ""
showStatus (x:xs) = case x of 
    Here -> "Y " 
    Nowhere -> "- "
    (Elsewhere Nothing) -> "y "
    (Elsewhere (Just Before)) -> "< "
    (Elsewhere (Just After)) -> "> "
    ++ showStatus xs

-- Part B, Iteration 2

updateAvailable :: [Char] -> [(Char, Status)] -> [Char]
updateAvailable chars [] = chars
updateAvailable chars ((chr, sts):ys) = updateAvailable (filter (\inp -> (inp /= chr)||(sts /= Nowhere)) chars) ys

-- Part C Iteration 3

leftMargin :: String
leftMargin = intercalate "" [ " " | _ <- [1..(length (prompt Start))]]

-- Part B, Iteration 1, 2 and 3

slice :: Int -> Int -> String -> String
slice i j word = take (j-i) (drop i word)

checkGuess :: String -> String -> [(Char, Status)]
checkGuess guess word = checkGuess' guess word 0

checkGuess' :: String -> String -> Int -> [(Char, Status)]
checkGuess' [] _ _ = []
checkGuess' _ [] _ = []
checkGuess' (x:xs) word cnt
    | ( x == (word!!cnt) ) = nextGuess Here
    | ( x `elem` (slice 0 cnt word) ) = nextGuess (Elsewhere (Just Before))
    | ( x `elem` (slice cnt (length word) word) ) = nextGuess (Elsewhere (Just After))
    | otherwise = nextGuess Nowhere
    where nextGuess status = (x, status) : checkGuess' xs word (cnt+1)

-- Part C, Iteration 1, 2 and 3

getGuess :: Int -> String -> IO String
getGuess n availableChars = getGuess' n 0 "" availableChars

getGuess' :: Int -> Int -> String -> String -> IO String
getGuess' n m cword availableChars 
    | n > 0 = do
        inp <- getChar'
        putStr " "
        inp <- if (inp `elem` availableChars) 
            then
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

-- Part D, Iteration 1, 2 & 3

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
        let checkedGuess = checkGuess userGuess word
        let guessStats = [s | (c, s) <- checkedGuess]
        let statuses = showStatus guessStats
        
        putStrLn (leftMargin++statuses)
        putChar '\n'

        if (and $ map (== Here) guessStats) then do
            putStrLn (prompt Win)
            return ()
        else do
            loop word (updateAvailable availableChars checkedGuess) (attemptN+1) (loopN-1)

-- Part E

strSplitter :: Char -> String -> [String]
strSplitter _ "" = []
strSplitter delim str = 
    let (start, rest) = break (== delim) str
        (_, remain) = span (== delim) rest
    in start : strSplitter delim remain

main :: IO ()
main = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    now <- getCurrentTime
    let (_, _, day) = toGregorian $ utctDay now
    wordlist <- (readFile "wordlist.txt") >>= (\inp -> return (strSplitter '\n' inp))
    go (wordlist!!(day `mod` 7))