import Data.Char
import Data.List

containsString :: String -> String -> Bool
containsString a = isInfixOf a

removeFirstWordTill :: Char -> String -> String
removeFirstWordTill _ [] = []
removeFirstWordTill a (b:bs)
  | a == b = bs
  | otherwise = removeFirstWordTill a bs

getFirstWordTill :: Char -> String -> String
getFirstWordTill _ [] = []
getFirstWordTill a (b:bs)
  | a == b || b == '}' = []
  | b == '{' = getFirstWordTill a bs
  | otherwise = [b] ++ (getFirstWordTill a bs)

showsUpNTimes :: Char -> String -> Int
showsUpNTimes _ [] = 0
showsUpNTimes a (b:bs)
  | a == b = 1 + showsUpNTimes a bs
  | otherwise = showsUpNTimes a bs

split :: String -> Char -> [String]
split [] _ = []
split a b = (getFirstWordTill b a):(split (removeFirstWordTill b a) b)

resolveAllChars :: String -> String
resolveAllChars [] = []
resolveAllChars (x:xs)
  | x == '{' || x == ',' = [] ++ resolveAllChars xs
  | x == '}' = []
  | otherwise = [x] ++ resolveAllChars xs

containsInString a b = containsString b a

biggerThanRule :: String -> String -> Bool
biggerThanRule all@(a:as) b
  | a == '{' = (showsUpNTimes (head as) b) > (showsUpNTimes (last all) b)
  | otherwise = (showsUpNTimes a b) > (showsUpNTimes (last all) b)

smallerThanRule :: String -> String -> Bool
smallerThanRule all@(a:as) b
  | a == '{' = (showsUpNTimes (head as) b) < (showsUpNTimes (last all) b)
  | otherwise = (showsUpNTimes a b) < (showsUpNTimes (last all) b)

modulusRule :: String -> String -> Bool
modulusRule a b
  | digitToInt (last a) > 0 = mod (showsUpNTimes (head a) b) (digitToInt (last a)) == 0
  | otherwise = False

numberOfTimesRule :: String -> String -> Bool
numberOfTimesRule a b = showsUpNTimes(head a) b == (digitToInt (last a))

evaluateRule :: String -> String -> Bool
evaluateRule a b
  | containsInA ">" = biggerThanRule a b
  | containsInA "<" = not (biggerThanRule a b)
  | containsInA "%" = modulusRule a b
  | containsInA "*" = containsInA (removeFirstWordTill '*' b)
  | containsInA ".n=" = numberOfTimesRule a b
  | otherwise = containsInA b
  where containsInA = containsInString a

verifyRules :: [String] -> String -> Bool
verifyRules [] _ = True
verifyRules (a:as) b = evaluateRule a b && verifyRules as b

verifyCharsInString :: String -> String -> Bool
verifyCharsInString _ [] = True
verifyCharsInString a (b:bs) = (elem b a) && verifyCharsInString a bs

validateLangFunc :: String -> String -> Bool
validateLangFunc a b = verifyCharsInString (resolveAllChars a) b && verifyRules (split (removeFirstWordTill ';' a) ',') b
