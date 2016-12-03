import Data.List.Extra

type Location = (Int, Int)
type Orientation = Int
type State = (Location, Orientation)
type Instruction = String

main :: IO ()
main = do
    content <- readFile "data.txt"
    let (finalLocation, _) = foldr processInstruction ((0, 0), 0) $ splitOn ", " (trim content)
    print $ locationNorm finalLocation

processInstruction :: Instruction -> State -> State
processInstruction instruction (loc, orient) =
    (nextLocation loc orient instruction, nextOrient)
        where nextOrient = nextOrientation orient instruction

nextLocation :: Location -> Orientation -> Instruction -> Location
nextLocation location orient (_:steps) = addOrientationToLocation location $ stepsInOrientation orient $ read steps

nextOrientation :: Orientation -> Instruction -> Orientation
nextOrientation orient ('L':_) = (orient - 1) `mod` 4
nextOrientation orient ('R':_) = (orient + 1) `mod` 4

orientation :: Orientation -> (Int, Int)
orientation 0 = (1, 0)
orientation 1 = (0, -1)
orientation 2 = (-1, 0)
orientation 3 = (0, 1)

stepsInOrientation :: Orientation -> Int -> (Int, Int)
stepsInOrientation orientationIndex steps = (x * steps, y * steps)
    where (x, y) = orientation orientationIndex

addOrientationToLocation :: Location -> (Int, Int) -> Location
addOrientationToLocation (x, y) (dx, dy) = (x + dx, y + dy)

locationNorm :: Location -> Int
locationNorm (x, y) = (abs x) + (abs y)
