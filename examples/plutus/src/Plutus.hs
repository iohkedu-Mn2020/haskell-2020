{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Plutus
    ( Natural, MonadError (..)
    , module Plutus.Value
    , ada, fromAda, adaAmount
    , PubKey, SlotEnd (..), SlotRange (..), srStart, srEnd
    , Address (..)
    , Datum, fromDynamic, toDyn,  unit
    , Script (..), Slot, TxId, tCurrencySymbol, tTokenName
    , Input (..), iTxId, iIx, iRedeemer
    , Output (..), oAddress, oValue, oDatum
    , Tx (..), txId, txInputs, txOutputs, txSignees, txSlotRange, txForge
    , ChainM, ChainError (..), ChainState, ValidationResult (..), fromEither, toEither
    , runChainM, uploadScript, tick, freshTxId, addTx
    , getRedeemer
    ) where

import           Control.Monad.Except
import           Control.Monad.State
import           Data.Dynamic
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.IntSet            (IntSet)
import qualified Data.IntSet            as IntSet
import           Numeric.Natural        (Natural)
import           Optics
import           Optics.State.Operators

import           Plutus.Value

-- Plan
--
-- - Reminder: (Simple) UTxO-model - DONE
-- - Extended UTxO-model - DONE
-- - Dynamics - DONE
-- - Commodities - DONE
-- - Monetary policies ("minting new tokens") - DONE
-- - Handling of time - DONE

type PubKey = String
type Slot = Natural

data SlotEnd = Finite Slot | Forever
    deriving (Show, Eq)

data SlotRange = SlotRange
    { _srStart :: Slot
    , _srEnd   :: SlotEnd
    } deriving Show

data ValidationResult =
      Validated
    | ValidationError String
    deriving (Show, Eq, Ord)

fromEither :: Either String () -> ValidationResult
fromEither (Left err) = ValidationError err
fromEither (Right ()) = Validated

toEither :: ValidationResult -> Either String ()
toEither (ValidationError err) = Left err
toEither Validated             = Right ()

type Datum = Dynamic

-- in real Plutus, something like:
-- data Datum =
--        DString String
--      | DInt Int
--      | DPubKey PubKey
--      | DHash Hash
--      | DPair Datum Datum
--      | DList [Datum]
--      | DUnit
--      | DMap String Datum-

newtype Script = Script {runScript :: ScriptId -> Value -> Datum -> Int -> [Output] -> Tx -> ValidationResult}

instance Show Script where
    show = const "Script"

data Address =
      PKAddress PubKey
    | ScriptAddress ScriptId
    deriving (Show, Eq)

type TxId = Int

data Output = Output
    { _oAddress :: Address
    , _oValue   :: Value
    , _oDatum   :: Datum
    } deriving Show

data Input = Input
    { _iTxId     :: TxId
    , _iIx       :: Int
    , _iRedeemer :: Datum
    } deriving Show

data Tx = Tx
    { _txId        :: Int
    , _txInputs    :: [Input]
    , _txOutputs   :: [Output]
    , _txSignees   :: [PubKey]
    , _txSlotRange :: SlotRange
    , _txForge     :: Value
    } deriving Show

data ChainState = ChainState
    { _csUTxOs   :: Map (TxId, Int) Output
    , _csTids    :: IntSet
    , _csSlot    :: Slot
    , _csScripts :: [Script]
    } deriving Show

makeLenses ''SlotRange
makeLenses ''Output
makeLenses ''Input
makeLenses ''Tx
makeLenses ''ChainState
makeLenses ''Value
makeLenses ''Token

inRange :: Slot -> SlotRange -> Bool
inRange s (SlotRange start mend) =
    s >= start &&
    case mend of
        Forever      -> True
        (Finite end) -> s <= end

unit :: Datum
unit = toDyn ()

ada :: Token
ada = Token (-1) "ada" -- there can never be a script with index (-1), so ada can never be forged

fromAda :: Natural -> Value
fromAda = fromToken ada

adaAmount :: Value -> Natural
adaAmount = tokenAmount ada

genesisState :: [(PubKey, Natural)] -> ChainState
genesisState xs = ChainState
    { _csUTxOs   = Map.fromList [((0, i), Output (PKAddress pk) (fromAda n) unit) | (i, (pk, n)) <- zip [0..] xs]
    , _csTids    = IntSet.singleton 0
    , _csSlot    = 0
    , _csScripts = []
    }

data ChainError =
      NonExistingOutput (TxId, Int)
    | MissingSignature PubKey
    | ValueMismatch Value Value
    | DuplicateTid TxId
    | ValidationError' String
    | SlotError Slot SlotRange
    | InvalidScriptId ScriptId
    | IllegalForging ScriptId
    deriving Show

newtype ChainM a = ChainM (StateT ChainState (Either ChainError) a)
    deriving (Functor, Applicative, Monad, MonadState ChainState, MonadError ChainError)

runChainM :: ChainM a -> [(PubKey, Natural)] -> Either ChainError (a, ChainState)
runChainM (ChainM m) = runStateT m . genesisState

tick :: Slot -> ChainM ()
tick slot = csSlot %= max slot

uploadScript :: Script -> ChainM ScriptId
uploadScript script = do
    csScripts %= (++ [script])
    (pred . length) <$> use csScripts

lookupScript :: ScriptId -> ChainM Script
lookupScript sid = do
    scripts <- use csScripts
    if sid < length scripts
        then return $ scripts !! sid
        else throwError $ InvalidScriptId sid

freshTxId :: ChainM TxId
freshTxId = do
    tids <- use csTids
    return $ head [tid | tid <- [0 ..], not (IntSet.member tid tids)]

addTx :: Tx -> ChainM ()
addTx tx = do
    let tid = tx ^. txId

    tids <- use csTids
    when (IntSet.member tid tids) $
        throwError $ DuplicateTid tid
    csTids %= IntSet.insert tid

    currentSlot <- use csSlot
    let sr = tx ^. txSlotRange
    unless (currentSlot `inRange` sr) $
        throwError $ SlotError currentSlot sr

    outputs <-  mapM validateInput1 $ tx ^. txInputs

    mapM_ (\(i, output)  -> validateInput2 tx i output outputs) (zip [0..] outputs)

    let inVal = mconcat $ (^. oValue) <$> outputs
    outVal <- mconcat <$> mapM (\(i, output) -> processOutput tid i output) (zip [0..] $ tx ^. txOutputs)
    let forge = tx ^. txForge
    when (inVal <> forge /= outVal) $
        throwError $ ValueMismatch inVal outVal

    let forgedCurrencySymbols = [cid | (Token cid _, _) <- Map.toList $ valueMap forge]
    forM_ forgedCurrencySymbols $ \cid ->
        unless (any (== ScriptAddress cid) $ (^. oAddress) <$> outputs) $
            throwError $ IllegalForging cid

validateInput1 :: Input -> ChainM Output
validateInput1 input = do

    let key = (input ^. iTxId, input ^. iIx)
    m <- use $ csUTxOs % at key
    output <- case m of
        Nothing -> throwError $ NonExistingOutput key
        Just o  -> return o
    csUTxOs % at key .= Nothing

    return output

validateInput2 :: Tx -> Int -> Output -> [Output] -> ChainM ()
validateInput2 tx i output outputs = do

    let v = output ^. oValue
        a = output ^. oAddress

    case a of
        PKAddress pk ->
            unless (pk `elem` tx ^. txSignees) $
                throwError $ MissingSignature pk
        ScriptAddress sid -> do
            script <- lookupScript sid
            case runScript script sid v (output ^. oDatum) i outputs tx of
                Validated           -> return ()
                ValidationError err -> throwError $ ValidationError' err

processOutput :: TxId -> Int -> Output -> ChainM Value
processOutput tid i output = do
    csUTxOs % at (tid, i) .= Just output
    return $ output ^. oValue

getRedeemer :: Int -> Tx -> Datum
getRedeemer index tx = case preview (txInputs % ix index % iRedeemer) tx of
    Nothing -> error "index out of range"
    Just r  -> r
