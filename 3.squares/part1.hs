import Data.List.Extra

main :: IO ()
main = do
    content <- readFile "data.txt"
    let possibleTriangles = map (map read . filter (not . null) . splitOn " ") $ lines $ trim content
    print $ length $ filter isTriangle possibleTriangles

isTriangle :: [Int] -> Bool
isTriangle [a, b, c]
    | (a + b > c) && (a + c > b) && (b + c > a) = True
    | otherwise = False
