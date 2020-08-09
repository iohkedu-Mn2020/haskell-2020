import Control.Monad      (forM_)
import Crypto.Hash        (hash, Digest, SHA256)
import Data.ByteString    (ByteString, readFile)
import Prelude            hiding (readFile)
import System.Directory   (doesFileExist, doesDirectoryExist)
import System.Environment (getArgs)

main :: IO ()
main = getArgs >>= sha256Sum

-- | Computes the SHA256 hash of a @`ByteString`@.
--
sha256' :: ByteString -> String
sha256' = (show :: Digest SHA256 -> String) . hash

-- | Prints the SHA256 hashes of the specified files
-- in the same format as the Linux utility `sha256sum`.
--
sha256Sum :: [FilePath] -> IO ()
sha256Sum fps = forM_ fps $ \fp -> do
  e <- doesFileExist fp
  if e
    then sha256SumExistingFile fp
    else sha256SumNonExistingFile fp

-- |Prints the SHA256 hash of an existing file
-- in the same format as the Linux utility `sha256sum`.
--
sha256SumExistingFile :: FilePath -> IO ()
sha256SumExistingFile fp = do
  file <- readFile fp
  putStrLn $ sha256' file ++ "  " ++ fp

-- |Prints the same message as the Linux utility `sha256sum`
-- for a non-existing file.
--
sha256SumNonExistingFile :: FilePath -> IO ()
sha256SumNonExistingFile fp = do
  d <- doesDirectoryExist fp
  let m = if d then "Is a directory" else "No such file or directory"
  putStrLn $ "sha256sum: " ++ fp ++ ": " ++ m
