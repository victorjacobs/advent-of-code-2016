import Data.List.Extra
import Data.Ord
import qualified Data.Map.Strict as Map

type CharacterOccurrences = Map.Map Char Int

main :: IO ()
main = do
    content <- readFile "data.txt"
    let ls = lines $ trim content
    print $ sortOccurrencesList $ occurrencesListForStrings ls

countCharacter :: Char -> CharacterOccurrences -> CharacterOccurrences
countCharacter chr = Map.insertWith (+) chr 1

newOccurrencesList :: [CharacterOccurrences]
newOccurrencesList = [Map.empty | _ <- [0..7]]

occurrencesListForString :: [CharacterOccurrences] -> String -> [CharacterOccurrences]
occurrencesListForString co str = zipWith countCharacter str co

occurrencesListForStrings :: [String] -> [CharacterOccurrences]
occurrencesListForStrings = foldl occurrencesListForString newOccurrencesList

sortOccurrencesList :: [CharacterOccurrences] -> String
sortOccurrencesList = map (fst . minimumBy (flip (comparing snd)) . Map.toList)
