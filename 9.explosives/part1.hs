import Data.List.Extra

main :: IO ()
main = do
    content <- readFile "data.txt"
    let ls = map (processLine 0) $ lines $ trim content
    print ls

processLine :: Int -> String -> Int
processLine count [] = count
processLine count ('(' : xs) = processLine (count + (chars * repeats)) rest
    where chars = read $ takeWhile (/= 'x') xs
          repeats = read $ takeWhile (/= ')') $ splitOn "x" xs !! 1
          rest = drop (4 + chars) xs
processLine count (_ : xs) = processLine (count + 1) xs
