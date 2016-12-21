import Text.Regex.PCRE
import Data.List.Extra

type Address = ([String], [String])

main :: IO ()
main = do
    content <- readFile "data.txt"
    let ls = filter supportsTLS $ map parseLine $ lines $ trim content
    print $ length ls

parseLine :: String -> Address
parseLine str = (firstAndLastPart ++ allOtherParts, hypernets)
    where hypernets = getAllTextMatches (str =~ "(?<=\\[)[a-z]+(?=\\])" :: AllTextMatches [] String)
          firstAndLastPart = [head $ splitOn "[" str, last $ splitOn "]" str]
          allOtherParts = map (replace "]" "" . replace "[" "") $ getAllTextMatches (str =~ "[\\]]{1}[a-z]+[\\[]{1}" :: AllTextMatches [] String)

containsABBA :: String -> Bool
containsABBA str@(a : b : c : d : _)
    | a == d && b == c && a /= b = True
    | length str == 4 = False
    | otherwise = containsABBA $ tail str

supportsTLS :: Address -> Bool
supportsTLS (parts, hypernets) = any containsABBA parts && not (any containsABBA hypernets)
