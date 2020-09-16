{-# LANGUAGE TemplateHaskell #-}

module Plutus.Examples where

import           Control.Monad
import qualified Data.Map               as Map
import           Optics

import           Plutus

genesis :: [(PubKey, Natural)]
genesis = [("Alice", 100), ("Bob", 50)]

always :: SlotRange
always = SlotRange 0 Forever

tx1 :: Tx
tx1 = Tx
    { _txId        = 1
    , _txInputs    = [Input 0 0 unit]
    , _txOutputs   = [Output (PKAddress "Alice") (fromAda 60) unit, Output (PKAddress "Bob") (fromAda 40) unit]
    , _txSignees   = ["Alice"]
    , _txSlotRange = always
    , _txForge     = mempty
    }

tx2 :: Tx
tx2 = Tx
    { _txId        = 2
    , _txInputs    = [Input 1 0 unit, Input 1 1 unit, Input 0 1 unit]
    , _txOutputs   = [ Output (PKAddress "Alice")   (fromAda   5) unit
                     , Output (PKAddress "Bob")     (fromAda  35) unit
                     , Output (PKAddress "Charlie") (fromAda 110) unit]
    , _txSignees   = ["Alice", "Bob"]
    , _txSlotRange = always
    , _txForge     = mempty
    }

aliceOrBob :: Script
aliceOrBob = Script $ \_addr _value _datum _index tx ->
  let
    xs = tx ^. txSignees
  in
    if "Alice" `elem` xs || "Bob" `elem` xs
        then Validated
        else ValidationError "Alice or Bob must sign the transaction!"

timeLock :: PubKey -> Slot -> Script
timeLock pk sl = Script $ \_addr _value _datum _index tx ->
    case (sl <= tx ^. txSlotRange % srStart,  pk `elem` tx ^. txSignees) of
        (True,  True)  -> Validated
        (False, True)  -> ValidationError "too early"
        (True,  False) -> ValidationError "recipient has not signed"
        (False, False) -> ValidationError "too early & recipient has not signed"

guessTheNumber :: Int -> Script
guessTheNumber n = Script $ \_addr _value _datum index tx ->
    case fromDynamic $ getRedeemer index tx of
        Nothing         -> ValidationError "wrong redeemer type"
        Just m
            | m == n    -> Validated
            | otherwise -> ValidationError "wrong number, try again!"

tx3 :: ChainM Tx
tx3 = do
    sid <- uploadScript aliceOrBob
    return Tx
        { _txId        = 1
        , _txInputs    = [Input 0 0 unit]
        , _txOutputs   = [Output (ScriptAddress sid) (fromAda 100) unit]
        , _txSignees   = ["Alice"]
        , _txSlotRange = always
        , _txForge     = mempty
        }

tx4 :: PubKey -> Tx
tx4 pk = Tx
    { _txId        = 2
    , _txInputs    = [Input 1 0 unit]
    , _txOutputs   = [Output (PKAddress pk) (fromAda 100) unit]
    , _txSignees   = [pk]
    , _txSlotRange = always
    , _txForge     = mempty
    }

tx5 :: SlotRange -> Tx
tx5 sr = Tx
    { _txId        = 1
    , _txInputs    = [Input 0 1 unit]
    , _txOutputs   = [Output (PKAddress "Charlie") (fromAda 50) unit]
    , _txSignees   = ["Bob"]
    , _txSlotRange = sr
    , _txForge     = mempty
    }

tx6 :: ChainM Tx
tx6 = do
    sid <- uploadScript $ timeLock "Charlie" 10
    return Tx
        { _txId        = 1
        , _txInputs    = [Input 0 1 unit]
        , _txOutputs   = [Output (ScriptAddress sid) (fromAda 50) unit]
        , _txSignees   = ["Bob"]
        , _txSlotRange = always
        , _txForge     = mempty
        }

tx7 :: PubKey -> SlotRange -> Tx
tx7 pk sr = Tx
    { _txId        = 2
    , _txInputs    = [Input 1 0 unit]
    , _txOutputs   = [Output (PKAddress pk) (fromAda 50) unit]
    , _txSignees   = [pk]
    , _txSlotRange = sr
    , _txForge     = mempty
    }

tx8 :: ChainM Tx
tx8 = do
    sid <- uploadScript $ guessTheNumber 42
    return Tx
        { _txId        = 1
        , _txInputs    = [Input 0 0 unit]
        , _txOutputs   = [Output (ScriptAddress sid) (fromAda 100) unit]
        , _txSignees   = ["Alice"]
        , _txSlotRange = always
        , _txForge     = mempty
        }

tx9 :: Datum -> Tx
tx9 redeemer = Tx
    { _txId        = 2
    , _txInputs    = [Input 1 0 redeemer]
    , _txOutputs   = [Output (PKAddress "Bob") (fromAda 100) unit]
    , _txSignees   = []
    , _txSlotRange = always
    , _txForge     = mempty
    }

-- Write a Plutus contract where a list of people can vote until a deadline for either Alice or Bob, and the person
-- that gets more votes gets the money. In case of a draw, the money goes to Charlie.

data VoteState = VoteState
    { _vsRemainingVoters :: [PubKey]
    , _vsVotesForAlice   :: Natural
    , _vsVotesForBob     :: Natural
    } deriving (Show, Eq)

makeLenses ''VoteState

data Vote = VoteForAlice | VoteForBob
    deriving (Show, Eq)

initialVoteState :: [PubKey] -> VoteState
initialVoteState voters = VoteState
    { _vsRemainingVoters = voters
    , _vsVotesForAlice   = 0
    , _vsVotesForBob     = 0
    }

-- 1000 ada                                                                    1000 ada           |              1000 ada
-- {["Charlie", "Doris", "Emilio"], 0, 0} ---- signed by Doris ---> {["Charlie", "Emilio"], 0, 1} | --- Bob ---> "Bob"
--                                               VoteForBob                                       |
--                                                                                              deadline

exampleVoting :: Either ChainError ((), ChainState)
exampleVoting = flip runChainM [("Charlie", 1000)] $ do
    sid <- uploadScript $ vote 10
    tick 1
    addTx Tx
        { _txId        = 1
        , _txInputs    = [Input 0 0 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromAda 1000
                            , _oDatum   = toDyn $ initialVoteState ["Charlie", "Doris", "Emilio"]
                            }]
        , _txSlotRange = SlotRange 1 $ Finite 1
        , _txForge     = mempty
        }
    tick 2
    addTx Tx
        { _txId        = 2
        , _txInputs    = [Input 1 0 $ toDyn VoteForBob]
        , _txSignees   = ["Doris"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromAda 1000
                            , _oDatum   = toDyn $ VoteState ["Charlie", "Emilio"] 0 1
                            }
                         ]
        , _txSlotRange = SlotRange 2 $ Finite 5
        , _txForge     = mempty
        }
    tick 10
    addTx Tx
        { _txId        = 3
        , _txInputs    = [Input 2 0 unit]
        , _txSignees   = ["Bob"]
        , _txOutputs   = [ Output (PKAddress "Bob") (fromAda 1000) unit ]
        , _txSlotRange = SlotRange 10 Forever
        , _txForge     = mempty
        }

vote :: Slot -> Script
vote deadline = Script validate
  where
    validate :: ScriptId -> Value -> Datum -> Int -> Tx -> ValidationResult
    validate sid value datum index tx = fromEither $ do

        vs <- case fromDynamic datum of
            Nothing -> throwError "expected VoteState"
            Just vs -> return vs

        let signees = tx ^. txSignees

        if tx ^. txSlotRange % srStart >= deadline
            then do
                let allowedToSign =
                        case compare (vs ^. vsVotesForAlice) (vs ^. vsVotesForBob) of
                            GT -> "Alice"
                            LT -> "Bob"
                            EQ -> "Charlies"
                unless (allowedToSign `elem` signees) $ throwError "not signed by the winner"

            else do
                pk <- case signees of
                    [pk] -> return pk
                    _    -> throwError "must be signed by exactly one voter"

                unless (pk `elem` vs ^. vsRemainingVoters) $ throwError "not entitled to vote"
                v <- case fromDynamic (getRedeemer index tx) of
                        Nothing -> throwError "redeemer must be a vote"
                        Just v  -> return v

                out <-  case tx ^. txOutputs of
                    [out] -> return out
                    _     -> throwError "expected exactly one output"

                unless (out ^. oValue   == value)              $ throwError "value not preserved"
                unless (out ^. oAddress == ScriptAddress sid)  $ throwError "wrong address"

                vs' <- case fromDynamic $ out ^. oDatum of
                    Nothing  -> throwError "output datum of wrong type"
                    Just vs' -> return vs'

                unless (vs' == expectedNewState vs pk v) $ throwError "wrong updated state"
      where
        expectedNewState :: VoteState -> PubKey -> Vote -> VoteState
        expectedNewState vs pk v =
            vs { _vsRemainingVoters = filter (/= pk) $ vs ^. vsRemainingVoters
               , _vsVotesForAlice   = if v == VoteForAlice then 1 + vs ^. vsVotesForAlice
                                                           else     vs ^. vsVotesForAlice
               , _vsVotesForBob     = if v == VoteForAlice then     vs ^. vsVotesForAlice
                                                           else 1 + vs ^. vsVotesForAlice
               }

-- challenge: how to only allow forging once
-- idea: tie forging to a resource that only exist once and is destroyed by forging.
-- Insist that a specific output is consumed in the forging transaction. Because this output will be gone
-- afterwards, no other forging transaction can ever exist.

charliesToken :: (TxId, Int) -> Script
charliesToken ptr = Script $ \sid _value _datum _index tx -> fromEither $ do

    unless ("Charlie" `elem` tx ^. txSignees) $
        throwError "only Charlie can forge this token"

    let xs = [ (tn, n)
             | (Token sid' tn, n) <- Map.toList $ valueMap $ tx ^. txForge
             , sid' == sid
             ]

    let tokenNames = fst <$> xs
    unless (tokenNames == [charliesTokenName]) $
        throwError $ "token names must be \"" ++ charliesTokenName ++ "\", but are " ++ show tokenNames

    unless (xs == [(charliesTokenName, 1000000)]) $
        throwError "exactly 1000000 tokens must be forged"

    unless (ptr `elem` [(tid, i) | (Input tid i _) <- tx ^. txInputs]) $
        throwError "required output is not consumed by the forging transaction"

charliesTokenName :: String
charliesTokenName = "Charlie's Token"

exampleForging :: Either ChainError ((), ChainState)
exampleForging = flip runChainM [("Charlie", 1000)] $ do
    cid <- uploadScript $ charliesToken (0, 0)
    let t = Token cid charliesTokenName
    addTx Tx
        { _txId        = 1
        , _txInputs    = []
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress cid
                            , _oValue   = mempty
                            , _oDatum   = unit
                            }]
        , _txSlotRange = always
        , _txForge     = mempty
        }
    addTx Tx
        { _txId        = 2
        , _txInputs    = [Input 1 0 unit, Input 0 0 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output
                            { _oAddress = PKAddress "Charlie"
                            , _oValue   = fromToken t 1000000
                            , _oDatum   = unit
                            }
                         , Output (PKAddress "Charlie") (fromAda 1000) unit
                         ]
        , _txSlotRange = always
        , _txForge     = fromToken t 1000000
        }


-- write a contract that allows anybody to offer some value for sale in exchange for a stated
-- amount of ada.

--  value: <1000 {0 Charlie's Token}>
--  datum: (100, PKAddress "Charlie")  --- Alice --->    to: Alice
--                                               |       value: <1000 0 {0 Charlie's Token}
--                                               |
--                                               |-->    to: Charlie
--                                                       value: 100 ada

sellValue :: Natural -> Script
sellValue _price = Script $ \_sid _value _datum _index _tx -> fromEither $ undefined

