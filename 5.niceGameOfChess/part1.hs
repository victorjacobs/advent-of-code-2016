import Crypto.Hash
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C

main :: IO ()
main = do
    content <- B.readFile "data.txt"
    print $ createPassword $ B.take 8 content

createPassword :: B.ByteString -> String
createPassword str = take 8 $ [h !! 5 | x <- [0..], let h = createHash x str, take 5 h == "00000"]

createHash :: Int -> B.ByteString -> String
createHash int str = show $ (hash $ B.append str $ C.pack (show int :: String) :: Digest MD5)
