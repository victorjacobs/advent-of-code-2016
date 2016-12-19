import Text.Regex.PCRE
import Data.List.Extra
import Data.Ord

type Address = ([String], [String])

main :: IO ()
main = do
    content <- readFile "data.txt"
    let ls = filter supportsTLS $ map parseLine $ lines $ trim content
    print $ length ls

parseLine :: String -> Address
parseLine str = (first, hyperNets)
    where hyperNets = getAllTextMatches (str =~ "(?<=\\[)[a-z]+(?=\\])" :: AllTextMatches [] String)
          first = head splitOnFirstBracket
          [hypernet, final] = splitOn "]" $ last splitOnFirstBracket

containsABBA :: String -> Bool
containsABBA str@(a : b : c : d : _)
    | a == d && b == c && a /= b = True
    | length str == 4 = False
    | otherwise = containsABBA $ tail str

supportsTLS :: Address -> Bool
supportsTLS (first, hypernet, final) = (containsABBA first || containsABBA final) && (not . containsABBA) hypernet
