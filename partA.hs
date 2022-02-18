import Base

-- iteration 1

showStatus :: [Status] -> String
showStatus [] = ""
showStatus (x:xs) = case x of 
    Here -> "Y " 
    Nowhere -> "- "
    (Elsewhere Nothing) -> "y "
    (Elsewhere (Just Before)) -> "< "
    (Elsewhere (Just After)) -> "> "
    ++ showStatus xs

-- iteration 2

updateAvailable :: [Char] -> [(Char, Status)] -> [Char]
updateAvailable chars [] = chars
updateAvailable chars ((chr, sts):ys) = updateAvailable (filter (\inp -> (inp /= chr)||(sts /= Nowhere)) chars) ys


