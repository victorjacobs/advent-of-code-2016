import Data.List.Extra
import qualified Data.Map.Strict as Map
import Data.Ord

type Room = (String, Int, String)
type CharOccurrences = Map.Map Char Int

main :: IO ()
main = do
    content <- readFile "data.txt"
    let ls = lines $ trim content
    print ls

parseLine :: String -> Room
parseLine str = (name, sector, checksum)
    where splitOnDashes = splitOn "-" str
          lastPartSplitOnBracket = splitOn "[" $ last splitOnDashes
          sector = read $ head lastPartSplitOnBracket
          checksum = init $ last lastPartSplitOnBracket
          name = intercalate "" $ init splitOnDashes

characterOccurrences :: String -> CharOccurrences -> CharOccurrences
characterOccurrences (x:xs) occurrences = characterOccurrences xs updatedOccurrences
    where updatedOccurrences = Map.insertWith (+) x 1 occurrences
characterOccurrences [] occurrences = occurrences
