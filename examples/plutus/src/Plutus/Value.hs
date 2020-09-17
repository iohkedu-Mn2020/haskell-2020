module Plutus.Value
    ( ScriptId, Token (..), Value
    , fromToken
    , valueMap
    , tokenAmount
    ) where

import           Data.List       (intercalate)
import           Data.Map        (Map)
import qualified Data.Map        as Map
import           Numeric.Natural (Natural)
import           Text.Printf     (printf)

type ScriptId = Int

data Token = Token
    { _tCurrencySymbol :: ScriptId
    , _tTokenName      :: String
    } deriving (Show, Eq, Ord)

newtype Value = Value (Map Token Natural)
    deriving Eq

instance Show Value where
    show (Value m) =
        "<" ++
        intercalate
            " + "
            [printf "%d %s" n (showToken t) | (t, n) <- Map.toList m] ++
        ">"
      where
        showToken :: Token -> String
        showToken (Token sid tn)
            | sid == (-1) = "ada"
            | otherwise   = printf "{%d %s}" sid tn

instance Semigroup Value where
    Value m <> Value n = Value $ Map.unionWith (+) m n
                                       -- fromList [("ada", 5), ("xyz", 7)] <> fromList [("ada", 6), ("abc", 3)]
                                       -- fromList [("ada", 11), ("xyz", 7), ("abc", 3)]

instance Monoid Value where
    mempty = Value Map.empty

fromToken :: Token -> Natural -> Value
fromToken t n
    | n == 0    = mempty
    | otherwise = Value $ Map.singleton t n

valueMap :: Value -> Map Token Natural
valueMap (Value m) = m

tokenAmount :: Token -> Value -> Natural
tokenAmount t (Value m) = Map.findWithDefault 0 t m
