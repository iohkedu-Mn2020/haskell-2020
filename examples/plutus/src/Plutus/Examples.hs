{-# LANGUAGE TemplateHaskell #-}

module Plutus.Examples where

import           Control.Monad
import qualified Data.Map        as Map
import           Optics          hiding (elements)
import           Test.QuickCheck

import           Plutus
import           Plutus.StateMachine

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
aliceOrBob = Script $ \_addr _value _datum _index _outputs tx ->
  let
    xs = tx ^. txSignees
  in
    if "Alice" `elem` xs || "Bob" `elem` xs
        then Validated
        else ValidationError "Alice or Bob must sign the transaction!"

timeLock :: PubKey -> Slot -> Script
timeLock pk sl = Script $ \_addr _value _datum _index _outputs tx ->
    case (sl <= tx ^. txSlotRange % srStart,  pk `elem` tx ^. txSignees) of
        (True,  True)  -> Validated
        (False, True)  -> ValidationError "too early"
        (True,  False) -> ValidationError "recipient has not signed"
        (False, False) -> ValidationError "too early & recipient has not signed"

guessTheNumber :: Int -> Script
guessTheNumber n = Script $ \_addr _value _datum index _outputs tx ->
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
    validate :: ScriptId -> Value -> Datum -> Int -> [Output] -> Tx -> ValidationResult
    validate sid value datum index _outputs tx = fromEither $ do

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
charliesToken ptr = Script $ \sid _value _datum _index _outputs tx -> fromEither $ do

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


-- write a contract that allows anybody to offer some value for sale in exchange for some price.
-- Also allow the seller to get the item on sale back at any time.

--
--  value: <1000 {0 Charlie's Token}>  --- Alice --->    to: Alice
--                                               |       value: <1000 0 {0 Charlie's Token}
--                                               |
--                                               |-->    to: Charlie
--                                                       value: 100 ada

sell :: Value -> PubKey -> Script
sell price seller = Script $ \_sid _value _datum _index _outputs tx -> fromEither $
    unless (seller `elem` tx ^. txSignees) $ do
        unless (any paysSeller $ tx ^. txOutputs) $ throwError "buyer doesn't pay seller"

  where
    paysSeller :: Output -> Bool
    paysSeller o = o ^. oValue   == price &&
                   o ^. oAddress == PKAddress seller

exampleSale :: Either ChainError ((), ChainState)
exampleSale = flip runChainM [("Alice", 1000), ("Bob", 1000), ("Charlie", 1000)] $ do
    cid <- uploadScript $ charliesToken (0, 2)
    let t = Token cid charliesTokenName
    addTx Tx -- creation of the currency symbol/ monetary policy for Charlie's Token
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
    addTx Tx -- forging of Charlie's Token
        { _txId        = 2
        , _txInputs    = [Input 1 0 unit, Input 0 2 unit]
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

    sellId <- uploadScript $ sell (fromAda 100) "Charlie"
    addTx Tx -- Charlie puts 1000 of his tokens on sale for 100 ada
        { _txId        = 3
        , _txInputs    = [Input 2 0 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sellId
                            , _oValue   = fromToken t 1000
                            , _oDatum   = unit
                            }
                         , Output (PKAddress "Charlie") (fromToken t 999000) unit
                         ]
        , _txSlotRange = always
        , _txForge     = mempty
        }
    addTx Tx -- Alice buys the 1000 tokens for 100 ada
        { _txId        = 4
        , _txInputs    = [Input 3 0 unit, Input 0 0 unit]
        , _txSignees   = ["Alice"]
        , _txOutputs   = [ Output (PKAddress "Alice")   (fromToken t 1000) unit
                         , Output (PKAddress "Charlie") (fromAda 100)      unit
                         , Output (PKAddress "Alice")   (fromAda 900)      unit
                         ]
        , _txSlotRange = always
        , _txForge     = mempty
        }
{-
    addTx Tx -- Charlies gets his tokens back (if nobody buys them)
        { _txId        = 4
        , _txInputs    = [Input 3 0 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output (PKAddress "Charlie") (fromToken t 1000) unit
                         ]
        , _txSlotRange = always
        , _txForge     = mempty
        }
-}


-- English Auction: An item is auctioned with some minimum bid. Bidders can raise the bid. After a deadline is reached,
-- the seller either can reclaim the item (if there was no bid) or either seller or highest bidder can settle the auction
-- by giving the item to the highest bidder and the highest bid to the seller.

-- Datum/State will be Maybe (PubKey, Natural) for the highest bidder and his or her bid (there may be none).

englishAuction :: PubKey -> Value -> Natural -> Slot -> Script
englishAuction seller item minBid deadline = Script $ \sid _value datum _index _outputs tx -> fromEither $ do

    mst <- case fromDynamic datum of
                Nothing  -> throwError "wrong type of datum"
                Just mst -> return mst

    if tx ^. txSlotRange % srStart >= deadline
        then case mst of
                Nothing            -> unless (seller `elem` tx ^. txSignees) $
                        throwError "only seller can reclaim the item"
                Just (bidder, bid) -> do
                    unless (any (\o -> o ^. oValue   == item &&             -- item is the value of this output
                                       o ^. oAddress == PKAddress bidder) $ -- the output goes to the bidder
                            tx ^. txOutputs) $
                        throwError "highest bidder must receive the item"
                    unless (any (\o -> o ^. oValue   == fromAda bid &&      -- bid is the value of this output
                                       o ^. oAddress == PKAddress seller) $ -- the output goes to the seller
                            tx ^. txOutputs) $
                        throwError "seller must receive the highest bid"

        else do
            bidder <- case tx ^. txSignees of
                [b] -> return b
                _   -> throwError "expected exactly one signature"
            o <- case filter (\o' -> o' ^. oAddress == ScriptAddress sid)  (tx ^. txOutputs) of
                [o'] -> return o'
                _    -> throwError "expected exactly one output at the auction script address"

            let newBid = adaAmount $ o ^. oValue
            unless (o ^. oValue == item <> fromAda newBid) $
                throwError "item must be contained in auction script output"

            case mst of
                Nothing                           ->
                    unless (newBid >= minBid) $
                        throwError "bid must be at least minimum bid"
                Just (previousBidder, previousBid) -> do
                    unless (newBid > previousBid) $
                        throwError "bid must be higher than current bid"
                    unless (any (\o' -> o' ^. oAddress == PKAddress previousBidder &&
                                        o' ^. oValue   == fromAda previousBid) $
                            tx ^. txOutputs) $
                        throwError "previous bidder must have his bid returned"

            mstNew <- case fromDynamic (o ^. oDatum) of
                Nothing      -> throwError "wrong type of datum in the auction script output"
                Just mstNew' -> return mstNew'

            unless (mstNew == Just (bidder, newBid)) $
                throwError "wrong state in auction script output"

startAuction :: ChainM (ScriptId, ScriptId)
startAuction = do
    cid <- uploadScript $ charliesToken (0, 2)
    let t = Token cid charliesTokenName
    addTx Tx -- creation of the currency symbol/ monetary policy for Charlie's Token
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
    addTx Tx -- forging of Charlie's Token
        { _txId        = 2
        , _txInputs    = [Input 1 0 unit, Input 0 2 unit]
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

    auctionScriptId <- uploadScript $ englishAuction "Charlie" (fromToken t 1000) 50 10
    addTx Tx -- puttin 1000 of Charlie's Token up for auction
        { _txId        = 3
        , _txInputs    = [Input 2 0 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress auctionScriptId
                            , _oValue   = fromToken t 1000
                            , _oDatum   = toDyn (Nothing :: Maybe (PubKey, Natural))
                            }
                         , Output (PKAddress "Charlie") (fromToken t 999000) unit
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    return (cid, auctionScriptId)

runAuction :: (ScriptId -> ScriptId -> ChainM a) -> Either ChainError (a, ChainState)
runAuction k = flip runChainM [("Alice", 1000), ("Bob", 1000), ("Charlie", 1000)] $ do
    (tsid, asid) <- startAuction
    k tsid asid

nobodyBids :: ScriptId -> ScriptId -> ChainM ()
nobodyBids tokenScriptId _auctionScriptId = do
    let t = Token tokenScriptId charliesTokenName
    tick 10
    addTx Tx -- reclaiming the 1000 tokens
        { _txId        = 4
        , _txInputs    = [Input 3 0 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output (PKAddress "Charlie") (fromToken t 1000) unit
                         ]
        , _txSlotRange = SlotRange 10 Forever
        , _txForge     = mempty
        }

aliceAndBobBid :: ScriptId -> ScriptId -> ChainM ()
aliceAndBobBid tokenScriptId auctionScriptId = do
    let t = Token tokenScriptId charliesTokenName
    addTx Tx -- Alice bids 50
        { _txId        = 4
        , _txInputs    = [Input 3 0 unit, Input 0 0 unit]
        , _txSignees   = ["Alice"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress auctionScriptId
                            , _oValue   = fromToken t 1000 <> fromAda 50
                            , _oDatum   = toDyn $ Just ("Alice", 50 :: Natural)
                            }
                         , Output (PKAddress "Alice") (fromAda 950) unit
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    addTx Tx -- Bob bids 60
        { _txId        = 5
        , _txInputs    = [Input 4 0 unit, Input 0 1 unit]
        , _txSignees   = ["Bob"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress auctionScriptId
                            , _oValue   = fromToken t 1000 <> fromAda 60
                            , _oDatum   = toDyn $ Just ("Bob", 60 :: Natural)
                            }
                         , Output (PKAddress "Alice") (fromAda 50) unit
                         , Output (PKAddress "Bob") (fromAda 940) unit
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    tick 10
    addTx Tx -- somebody settles the auction
        { _txId        = 6
        , _txInputs    = [Input 5 0 unit]
        , _txSignees   = []
        , _txOutputs   = [ Output (PKAddress "Bob") (fromToken t 1000) unit
                         , Output (PKAddress "Charlie") (fromAda 60) unit
                         ]
        , _txSlotRange = SlotRange 10 Forever
        , _txForge     = mempty
        }

data ElevatorTransition = Up | Down -- elevator can change (transition) state by either going up or down
    deriving Show

elevator :: StateMachine Int ElevatorTransition
elevator = StateMachine
    { initialState = 1
    , transit      = elevatorTransit
    , isFinal      = const False
    }
  where
    elevatorTransit :: Int -> ElevatorTransition -> Maybe Int
    elevatorTransit flr Up
        | flr < 10   = Just $ flr + 1
        | otherwise  = Nothing
    elevatorTransit flr Down
        | flr > 1    = Just $ flr - 1
        | otherwise  = Nothing

exampleElevator :: Either ChainError ((), ChainState)
exampleElevator = flip runChainM [] $ do
    (sid, tid, token) <- deployStateMachine elevator trivialCont "Alice" -- initialize elevator at floor 1

    addTx Tx                                  -- move elevator up
        { _txId        = tid + 1
        , _txInputs    = [Input tid 0 $ toDyn Up]
        , _txSignees   = []
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromToken token 1
                            , _oDatum   = toDyn (2 :: Int)
                            }
                         ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = mempty
        }

instance Arbitrary ElevatorTransition where
    arbitrary = elements [Up, Down]
    shrink _  = []

applyTransitions :: Int -> [ElevatorTransition] -> Int
applyTransitions flr []       = flr
applyTransitions flr (t : ts) = case transit elevator flr t of
    Nothing   -> applyTransitions flr  ts
    Just flr' -> applyTransitions flr' ts

prop_ElevatorDoesNotFallThroughTheBottom :: [ElevatorTransition] -> Property
prop_ElevatorDoesNotFallThroughTheBottom ts =
    let flr' = applyTransitions 1 ts
    in  counterexample (show flr') $ flr' >= 1

prop_ElevatorDoesNotGoThroughTheRoof :: [ElevatorTransition] -> Property
prop_ElevatorDoesNotGoThroughTheRoof ts =
    let flr' = applyTransitions 1 ts
    in  counterexample (show flr') $ flr' <= 10




-- voting via state machines

-- We have a list of voters, two candidates, a deadline and some amount of ada. Voters can vote for
-- one of the two candidates. Onces the deadline is reached, the winner gets the ada. In case of a draw,
-- the money is split "evenly".

data VotingState =
      NotFunded
    | VotingPhase [PubKey] Int Int
    | Ended
    deriving Show

data VotingTransition =
      ProvideFunding
    | VoteForFirstCandidate PubKey
    | VoteForSecondCandidate PubKey
    | PayWinner
    deriving Show

isFinalVotingState :: VotingState -> Bool
isFinalVotingState Ended = True
isFinalVotingState _     = False

transitVoting :: [PubKey] -> VotingState -> VotingTransition -> Maybe VotingState
transitVoting voters NotFunded             ProvideFunding             = Just $ VotingPhase voters 0 0
transitVoting _      (VotingPhase pks m n) (VoteForFirstCandidate pk)
    | pk `elem` pks                                                   =
        Just $ VotingPhase (filter (/= pk) pks) (m + 1) n
transitVoting _      (VotingPhase pks m n) (VoteForSecondCandidate pk)
    | pk `elem` pks                                                   =
        Just $ VotingPhase (filter (/= pk) pks) m (n + 1)
transitVoting _      (VotingPhase _ _ _)   PayWinner                  = Just Ended
transitVoting _      _                     _                          = Nothing

voting :: [PubKey] -> StateMachine VotingState VotingTransition
voting voters = StateMachine
    { initialState = NotFunded
    , transit      = transitVoting voters
    , isFinal      = isFinalVotingState
    }

votingCont :: PubKey                      -- ^ first candidate
           -> PubKey                      -- ^ second candidate
           -> Natural                     -- ^ required funding in ada
           -> Slot                        -- ^ deadline
           -> VotingState
           -> VotingTransition
           -> Maybe (VotingState, Output)
           -> Script
votingCont _ _ amount _ _ ProvideFunding (Just (_, o)) = Script $ \_sid _value _datum _index _outputs _tx -> fromEither $
    unless (tokenAmount ada (o ^. oValue) == amount) $
        throwError "funding not provided"
votingCont _ _ amount _ _ (VoteForFirstCandidate pk) (Just (_, o)) = Script $ \_sid _value _datum _index _outputs tx -> fromEither $ do
    unless (pk `elem` tx ^. txSignees) $
        throwError "voter must sign transaction"
    unless (tokenAmount ada (o ^. oValue) == amount) $
        throwError "funding not provided"
votingCont _ _ amount _ _ (VoteForSecondCandidate pk) (Just (_, o)) = Script $ \_sid _value _datum _index _outputs tx -> fromEither $ do
    unless (pk `elem` tx ^. txSignees) $
        throwError "voter must sign transaction"
    unless (tokenAmount ada (o ^. oValue) == amount) $
        throwError "funding not provided"
votingCont cm cn amount deadline (VotingPhase _ m n) PayWinner Nothing = Script $ \_sid _value _datum _index _outputs tx -> fromEither $ do
    unless (tx ^. txSlotRange % srStart >= deadline) $
        throwError "deadline has not been reached"
    if m > n
        then
            unless (cm `elem` tx ^. txSignees) $
                throwError "first candidate must sign the transaction"
        else if n > m
            then
                unless (cn `elem` tx ^. txSignees) $
                    throwError "second candidate must sign the transaction"
            else do
                let am = amount `div` 2
                    an = amount - am
                unless (any (\o -> o ^. oAddress == PKAddress cm &&
                                   tokenAmount ada (o ^. oValue) == am) $ tx ^. txOutputs) $
                    throwError "first candidate must be paid"
                unless (any (\o -> o ^. oAddress == PKAddress cn &&
                                   tokenAmount ada (o ^. oValue) == an) $ tx ^. txOutputs) $
                    throwError "second candidate must be paid"
votingCont _ _ _ _ _ _ _ = Script $ \_ _ _ _ _ _ -> ValidationError "unexpected situation"
















--  contract address: A       /-> Tx2' ---> O3'
--                           /
--                          /
--  ---> O1 ---> Tx1 ---> O2 ---> Tx2  ---> O3
--       A                A                 A
--       s1               s2                s3

-- PROBLEM: In order to transition from a state s(i) to a state s(i+1),
-- you have to create a transaction that consumes O(i) and produces an new
-- output O(i+1). This transaction will FAIL if somebody else has updated the state
-- in the time it took you to create and submit that transaction.
-- So if there is a Tx2' that consumes O2, Tx2 will be invalid.








-- crowd sourcing campaign
--
-- Same as in the Marlowe assignment, except no fixed list of contributors and no fixed individual
-- contribution.
--
-- Idea: Create a smart contract for the campaign. Contributors contribute by sending money
-- to the address of that smart contract. There are two ways to unlock the money:
--  - the campaign owner can unlock it if the campaign goal has been reached.
--  - contirbutors can unlock it if not.
--
-- Introduce two deadline: After the first, the owner can consume the outputs in one transaction if the sum of the input
-- values is at least as large as the campaign goal. After the second deadline, every contributor can get his or her owen
-- contribution back.
--
--                      | first deadline            | second deadline
--
--      O1 -----------
--     500            \
--     Alice           |
--                      \
--         O2 -----------> TX ->  O
--        400     /      Owner  1900
--        Bob    /
--              /
--      O3 -----
--    1000
--    Charlie
--
--
--      O1 ----------------------------------------> TX -> O3
--     500                                                500
--
--         O2 -------------------------------------> TX ->  O4
--        400                                              400
--
-- use the contributor as datum for the output

crowdSourcingCampaign :: PubKey -> Natural -> Slot -> Slot -> Script
crowdSourcingCampaign owner goal deadline1 deadline2 = Script $ \sid _value datum _index outputs tx -> fromEither $
    if owner `elem` tx ^. txSignees
        then do
            unless (tx ^. txSlotRange % srStart >= deadline1) $ -- check that deadline 1 has passed
                throwError "too early for collection"

            let sumOfAda = sum [adaAmount $ o ^. oValue | o <- outputs, (o ^. oAddress) == ScriptAddress sid]
            unless (sumOfAda >= goal) $
                throwError "campaign goal has not been reached"

        else do
            unless (tx ^. txSlotRange % srStart >= deadline2) $ -- check that deadline 2 has passed
                throwError "too early for refunds"
            contributor <- case fromDynamic datum of
                Nothing -> throwError "datum has the wrong type"-- check that datum contains a public key
                Just pk -> return pk
            unless (contributor `elem` tx ^. txSignees) $       -- check that public key from the datum has signed the tx
                throwError "contributor must sign refund transaction"

successfulCampaignExample :: Either ChainError ((), ChainState)
successfulCampaignExample = flip runChainM [("Alice", 1000), ("Bob", 1000), ("Charlie", 1000)] $ do
    sid <- uploadScript $ crowdSourcingCampaign "Andres" 1800 10 20
    addTx Tx -- Alice contributes 500
        { _txId        = 1
        , _txInputs    = [Input 0 0 unit]
        , _txSignees   = ["Alice"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromAda 500
                            , _oDatum   = toDyn "Alice"
                            }
                         , Output (PKAddress "Alice") (fromAda 500) unit
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    addTx Tx -- Bob contributes 400
        { _txId        = 2
        , _txInputs    = [Input 0 1 unit]
        , _txSignees   = ["Bob"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromAda 400
                            , _oDatum   = toDyn "Bob"
                            }
                         , Output (PKAddress "Bob") (fromAda 600) unit
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    addTx Tx -- Charlies contributes 1000
        { _txId        = 3
        , _txInputs    = [Input 0 2 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromAda 1000
                            , _oDatum   = toDyn "Charlie"
                            }
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    tick 10
    addTx Tx -- Andres collects
        { _txId        = 4
        , _txInputs    = [Input 1 0 unit, Input 2 0 unit, Input 3 0 unit]
        , _txSignees   = ["Andres"]
        , _txOutputs   = [ Output (PKAddress "Andres") (fromAda 1900) unit ]
        , _txSlotRange = SlotRange 10 (Finite 10)
        , _txForge     = mempty
        }

failingCampaignExample :: Either ChainError ((), ChainState)
failingCampaignExample = flip runChainM [("Alice", 1000), ("Bob", 1000), ("Charlie", 1000)] $ do
    sid <- uploadScript $ crowdSourcingCampaign "Andres" 1800 10 20
    addTx Tx -- Alice contributes 500
        { _txId        = 1
        , _txInputs    = [Input 0 0 unit]
        , _txSignees   = ["Alice"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromAda 300
                            , _oDatum   = toDyn "Alice"
                            }
                         , Output (PKAddress "Alice") (fromAda 700) unit
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    addTx Tx -- Bob contributes 400
        { _txId        = 2
        , _txInputs    = [Input 0 1 unit]
        , _txSignees   = ["Bob"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromAda 400
                            , _oDatum   = toDyn "Bob"
                            }
                         , Output (PKAddress "Bob") (fromAda 600) unit
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    addTx Tx -- Charlies contributes 1000
        { _txId        = 3
        , _txInputs    = [Input 0 2 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress sid
                            , _oValue   = fromAda 1000
                            , _oDatum   = toDyn "Charlie"
                            }
                         ]
        , _txSlotRange = SlotRange 0 (Finite 0)
        , _txForge     = mempty
        }
    tick 20
    addTx Tx -- Alice gets refund
        { _txId        = 4
        , _txInputs    = [Input 1 0 unit]
        , _txSignees   = ["Alice"]
        , _txOutputs   = [ Output (PKAddress "Alice") (fromAda 300) unit ]
        , _txSlotRange = SlotRange 20 (Finite 20)
        , _txForge     = mempty
        }
    addTx Tx -- Bob gets refund
        { _txId        = 5
        , _txInputs    = [Input 2 0 unit]
        , _txSignees   = ["Bob"]
        , _txOutputs   = [ Output (PKAddress "Bob") (fromAda 400) unit ]
        , _txSlotRange = SlotRange 20 (Finite 20)
        , _txForge     = mempty
        }
    addTx Tx -- Charlie gets refund
        { _txId        = 6
        , _txInputs    = [Input 3 0 unit]
        , _txSignees   = ["Charlie"]
        , _txOutputs   = [ Output (PKAddress "Charlie") (fromAda 1000) unit ]
        , _txSlotRange = SlotRange 20 (Finite 20)
        , _txForge     = mempty
        }
