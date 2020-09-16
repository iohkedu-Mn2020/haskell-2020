import Data.Maybe (fromMaybe)
import W0403

main :: IO ()
main = do
    mini <- getLine
    putStrLn $ fromMaybe "INVALID" $ validate mini
