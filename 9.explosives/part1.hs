import Data.List.Extra

main :: IO ()
main = do
    content <- readFile "data.txt"
    let ls = map (processLine 0) $ lines $ trim content
    print ls

processLine :: Int -> String -> Int
processLine count [] = count
processLine count ('(' : xs) = processLine (count + (charsParsed * repeatsParsed)) rest
    where chars = takeWhile (/= 'x') xs
          charsParsed = read chars
          repeats = takeWhile (/= ')') $ splitOn "x" xs !! 1
          repeatsParsed = read repeats
          instructionLength = length chars + length repeats + 2
          rest = drop (instructionLength + charsParsed) xs
processLine count (_ : xs) = processLine (count + 1) xs
