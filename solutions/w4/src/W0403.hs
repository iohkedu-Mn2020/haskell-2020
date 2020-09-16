-- | W4.3 Mini private keys
module W0403
    ( -- * Subtask W4.3.1
      validate
      -- * Subtask W4.3.2
    , Net (..), Mode (..), WalletParams (..)
    , toWallet, toWalletIO
      -- * Subtask W4.3.3
    , fromWallet, fromWalletIO
    ) where

import           Control.Monad          (guard)
import           Crypto.Hash            (Digest, SHA256, hash)
import qualified Data.ByteArray         as A
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as B
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Base58 as B58
import qualified Data.ByteString.Char8  as C
import           Data.Maybe             (fromMaybe)
import           Data.Word              (Word8)
import           Options.Applicative

-- Subtask W4.3.1

-- | Validates a mini private key (given as a @'String'@), returns @'Nothing'@ if the key is invalid,
-- otherwise @'Just'@ the private key.
--
-- >>> validate "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy"
-- Just "4c7a9640c72dc2099f23715d0c8a0d8a35f8906e3cab61dd3f78b67bf887c9ab"

-- >>> validate "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRz"
-- Nothing
--
validate :: String -> Maybe String
validate mini =
    let bs = C.pack mini
    in  if isValid bs
            then Just $ C.unpack $ B16.encode $ sha256 bs
            else Nothing

-- | Computes the SHA256 hash of the given @'ByteString'@.
sha256 :: ByteString -> ByteString
sha256 bs = let digest = hash bs :: Digest SHA256 in B.pack $ A.unpack digest

-- |Checks whether a mini private key (given as a @'ByteString'@)
-- is valid.
--
-- >>> isValid $ C.pack "S6c56bnXQiBjk9mqSYE7ykVQ7NzrRy"
-- True
isValid :: ByteString -> Bool
isValid bs
    | B.length bs /= 30 = False
    | otherwise         =
        let bs'     = bs `C.snoc` '?'
            d       = B.head $ sha256 bs' -- Taking the head is okay, because sha256 always produces 256 bits.
        in  d == 0

-- Subtask W4.3.2

-- | The targeted network.
data Net = Main | Test
    deriving (Show, Read, Eq)

-- | The public key mode.
data Mode = Compressed | Uncompressed
    deriving (Show, Read, Eq)

-- | A type representing to possible configurations for conversion to- and from
-- wallet import format.
data WalletParams = WalletParams Net Mode
    deriving (Show, Read, Eq)

-- | Tries to convert a private key to wallet input format.
--
-- >>> toWallet (WalletParams Main Uncompressed) "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
-- Just "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ"
--
-- >>> toWallet (WalletParams Main Uncompressed) "1111111111111111111111111111111111111111111111111111111111111111"
-- Just "5HwoXVkHoRM8sL2KmNRS217n1g8mPPBomrY7yehCuXC1115WWsh"
--
-- >>> toWallet (WalletParams Main Compressed) "1111111111111111111111111111111111111111111111111111111111111111"
-- Just "KwntMbt59tTsj8xqpqYqRRWufyjGunvhSyeMo3NTYpFYzZbXJ5Hp"
--
-- >>> toWallet (WalletParams Test Uncompressed) "1111111111111111111111111111111111111111111111111111111111111111"
-- Just "91iS7EZqPeRGqPXcPiKLtbfjfLVUYYj17oQ54H3iFFw3n1UmZSS"
--
-- >>> toWallet (WalletParams Test Compressed) "1111111111111111111111111111111111111111111111111111111111111111"
-- Just "cN9spWsvaxA8taS7DFMxnk1yJD2gaF2PX1npuTpy3vuZFJdwavaw"
--
toWallet :: WalletParams -> String -> Maybe String
toWallet (WalletParams net mode) s = do
    let (pk, r) = B16.decode $ C.pack s
    guard $ B.null r
    let pk'  = addSuffix mode $ B.cons (prefix net) pk
        h1   = sha256 pk'
        h2   = sha256 h1
        cs   = B.take 4 h2
        pk'' = pk' `B.append` cs
        w    = B58.encodeBase58 B58.bitcoinAlphabet pk''
    return $ C.unpack w

toWalletIO :: IO ()
toWalletIO = do
    wp <- execParser toWalletParams
    s  <- getLine
    putStrLn $ fromMaybe "INVALID" $ toWallet wp s

-- | Gets the prefix for the specified @'Net'@ in the extended private key.
prefix :: Net -> Word8
prefix Main = 0x80
prefix Test = 0xef

-- | The suffix to append to a private key in case of a /compressed/ public key.
compressedSuffix :: Word8
compressedSuffix = 0x01

-- | Given a mode and a private key, adds the suffix corresponding to the
-- @'Mode'@ (i.e. does nothing for @'Uncompressed'@ and appends a @0x01@
-- for @'Compressed'@).
addSuffix :: Mode -> ByteString -> ByteString
addSuffix Compressed bs   = B.snoc bs compressedSuffix
addSuffix Uncompressed bs = bs

-- Subtask W4.3.3

-- | Tries to convert from wallet input format to a private key.
--
-- >>> fromWallet (WalletParams Main Uncompressed) "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTJ"
-- Just "0c28fca386c7a227600b2fe50b7cae11ec86d3bf1fbe471be89827e19d72aa1d"
--
-- >>> fromWallet (WalletParams Main Uncompressed) "5HwoXVkHoRM8sL2KmNRS217n1g8mPPBomrY7yehCuXC1115WWsh"
-- Just "1111111111111111111111111111111111111111111111111111111111111111"
--
-- >>> fromWallet (WalletParams Main Compressed) "KwntMbt59tTsj8xqpqYqRRWufyjGunvhSyeMo3NTYpFYzZbXJ5Hp"
-- Just "1111111111111111111111111111111111111111111111111111111111111111"
--
-- >>> fromWallet (WalletParams Test Uncompressed) "91iS7EZqPeRGqPXcPiKLtbfjfLVUYYj17oQ54H3iFFw3n1UmZSS"
-- Just "1111111111111111111111111111111111111111111111111111111111111111"
--
-- >>> fromWallet (WalletParams Test Compressed) "cN9spWsvaxA8taS7DFMxnk1yJD2gaF2PX1npuTpy3vuZFJdwavaw"
-- Just "1111111111111111111111111111111111111111111111111111111111111111"
--
-- >>> fromWallet (WalletParams Main Uncompressed) "5HueCGU8rMjxEXxiPuD5BDku4MkFqeZyd4dZ1jvhTVqvbTLvyTj"
-- Nothing
--
fromWallet :: WalletParams -> String -> Maybe String
fromWallet (WalletParams net mode) s = do
    bs <- B58.decodeBase58 B58.bitcoinAlphabet $ C.pack s
    let l = B.length bs
    guard $ l >= 5
    guard $ B.head bs == prefix net
    let (bs', cs) = B.splitAt (l - 4) bs
        h1        = sha256 bs'
        h2        = sha256 h1
        cs'       = B.take 4 h2
    guard $ cs == cs'
    let pk = B.tail bs'
    pk' <- stripSuffix mode pk
    return $ C.unpack $ B16.encode pk'

fromWalletIO :: IO ()
fromWalletIO = do
    wp <- execParser fromWalletParams
    s  <- getLine
    putStrLn $ fromMaybe "INVALID" $ fromWallet wp s

-- | Given a mode and a private key, tries to strip the suffix corresponding to the
-- @'Mode'@ (i.e. does nothing for @'Uncompressed'@ and strips a @0x01@
-- for @'Compressed'@).
stripSuffix :: Mode -> ByteString -> Maybe ByteString
stripSuffix Compressed bs   = case B.unsnoc bs of
    Nothing                     -> Nothing
    Just (bs', b)
        | b == compressedSuffix -> Just bs'
        | otherwise             -> Nothing
stripSuffix Uncompressed bs = Just bs

-- | A commandline parser for @'WalletParams'@.
params :: Parser WalletParams
params = WalletParams
    <$> option auto
        (  long "net"
        <> short 'n'
        <> help "the network to use ('Main' or 'Test')"
        <> showDefault
        <> value Main
        <> metavar "NET"
        )
    <*> option auto
        (  long "mode"
        <> short 'm'
        <> help "the public key mode ('Compressed' or 'Uncompressed')"
        <> showDefault
        <> value Uncompressed
        <> metavar "MODE"
        )

-- | A @'ParserInfo'@ specialized to the @w040302@ tool.
toWalletParams :: ParserInfo WalletParams
toWalletParams = info (params <**> helper)
    (  fullDesc
    <> progDesc "Converts a private key from standard input to wallet import format, targeting NET and public keys of type MODE."
    <> header "w040302 - converting a private key to wallet import format"
    )

-- | A @'ParserInfo'@ specialized to the @w040303@ tool.
fromWalletParams :: ParserInfo WalletParams
fromWalletParams = info (params <**> helper)
    (  fullDesc
    <> progDesc "Converts wallet import format from standard input to a private key, targeting NET and public keys of type MODE."
    <> header "w040303 - converting wallet import format to a private key"
    )
