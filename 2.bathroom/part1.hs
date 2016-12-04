import Data.List.Extra

type State = Int

main :: IO ()
main = do
    content <- readFile "data.txt"
    let steps = scanr processLine 4 $ lines $ trim content
    print steps

processLine :: String -> State -> State
processLine line state = foldl processInstruction state line

processInstruction :: State -> Char -> State
processInstruction state instruction
    | (instruction == 'U') && (state >= 3) = state - 3
    | (instruction == 'D') && (state <= 5) = state + 3
    | (instruction == 'L') && (state /= 0) && (state /= 3) && (state /= 6) = state - 1
    | (instruction == 'R') && (state /= 2) && (state /= 5) && (state /= 8) = state + 1
    | otherwise = state
