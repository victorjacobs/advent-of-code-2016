import Data.List.Extra
import qualified Data.Map.Strict as Map

type Room = (String, Int, String)
type CharOccurrences = Map.Map Char Int

main :: IO ()
main = do
    content <- readFile "data.txt"
    let ls = foldl (+) 0 $ map sectorId $ filter isCorrectChecksum $ map parseLine $ lines $ trim content
    print ls

parseLine :: String -> Room
parseLine str = (name, sector, chk)
    where splitOnDashes = splitOn "-" str
          lastPartSplitOnBracket = splitOn "[" $ last splitOnDashes
          sector = read $ head lastPartSplitOnBracket
          chk = init $ last lastPartSplitOnBracket
          name = intercalate "" $ init splitOnDashes

isCorrectChecksum :: Room -> Bool
isCorrectChecksum (name, _, chk) = (checksum $ characterOccurrences name Map.empty) == chk

sectorId :: Room -> Int
sectorId (_, sid, _) = sid

characterOccurrences :: String -> CharOccurrences -> CharOccurrences
characterOccurrences (x:xs) occurrences = characterOccurrences xs updatedOccurrences
    where updatedOccurrences = Map.insertWith (+) x 1 occurrences
characterOccurrences [] occurrences = occurrences

checksum :: CharOccurrences -> String
checksum occurrences = take 5 $ foldr ((++) . sort) "" $ map snd $ Map.toDescList $ Map.foldlWithKey insertCharacterByCount Map.empty occurrences

insertCharacterByCount :: Map.Map Int String -> Char -> Int -> Map.Map Int String
insertCharacterByCount charsByCount char count = Map.insertWith (++) count [char] charsByCount
