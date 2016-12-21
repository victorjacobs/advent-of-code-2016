import Text.Regex.PCRE
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
    let ls = lines $ trim content
    print $ length ls

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
evaluateInstruction state (Rectangle x2 y2) =
    foldr (Map.alter (\_ -> Just True)) state [(x, y) | x <- [0..(x2 - 1)], y <- [0..(y2 - 1)]]
-- evaluateInstruction state (RotateColumn pos amount) =
