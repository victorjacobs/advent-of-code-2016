import Data.List.Extra
import qualified Data.Map.Strict as Map

type Pos = (Int, Int)
type State = Map.Map Pos Bool

data Instruction =
    RotateRow Int Int |
    RotateColumn Int Int |
    Rectangle Int Int
    deriving (Show)

main :: IO ()
main = do
    content <- readFile "data.txt"
    let finalState = foldl evaluateInstruction Map.empty $ map parseLine $ lines $ trim content
    print $ length $ Map.filter id finalState

parseLine :: String -> Instruction
parseLine str = parseLineParts $ splitOn " " str

parseLineParts :: [String] -> Instruction
parseLineParts ["rect", params] = Rectangle x y
    where [x, y] = map read $ splitOn "x" params
parseLineParts ["rotate", direction, position, _, amount]
    | direction == "column" = RotateColumn posParsed amountParsed
    | direction == "row" = RotateRow posParsed amountParsed
        where posParsed = read $ last $ splitOn "=" position
              amountParsed = read amount

evaluateInstruction :: State -> Instruction -> State
evaluateInstruction state (Rectangle x2 y2) = foldr (`Map.insert` True) state [(x, y) | x <- [0..(x2 - 1)], y <- [0..(y2 - 1)]]
evaluateInstruction state (RotateRow row amount) = foldl (rotateRowElement amount) state rowElements
    where rowElements = [(pos, Map.lookup pos state) | x <- [0..49], let pos = (x, row)]
evaluateInstruction state (RotateColumn column amount) = foldl (rotateColumnElement amount) state columnElements
    where columnElements = [(pos, Map.lookup pos state) | y <- [0..5], let pos = (column, y)]

rotateRowElement :: Int -> State -> (Pos, Maybe Bool) -> State
rotateRowElement amount state ((x, y), Just bool) = Map.insert ((x + amount) `mod` 50, y) bool state
rotateRowElement amount state ((x, y), Nothing) = Map.insert ((x + amount) `mod` 50, y) False state

rotateColumnElement :: Int -> State -> (Pos, Maybe Bool) -> State
rotateColumnElement amount state ((x, y), Just bool) = Map.insert (x, (y + amount) `mod` 6) bool state
rotateColumnElement amount state ((x, y), Nothing) = Map.insert (x, (y + amount) `mod` 6) False state
