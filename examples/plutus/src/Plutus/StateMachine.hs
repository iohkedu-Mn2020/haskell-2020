{-# LANGUAGE ScopedTypeVariables #-}

module Plutus.StateMachine where

import Control.Monad
import Data.Typeable (Typeable)
import Optics

import Plutus

{-
-- State Machines:
--
-- A "machine" that has a state of some type and can undergo transations of a specific type
-- which change the state.
--
-- Described by:
--
-- - a type s for the state
-- - a type t for the possible transitions
-- - a transition function s -> t -> Maybe s (which either gives the new state or indicates that the transition is illegal
--   for that state.
-- - an initial state
-- - optionally a set of final states, given by a predicate s -> Bool which indicates whether a state is final. In a final state
--   no more transitions are possible.


-- Remember the elevator DSL: The elevator can go up (if not at the top) or down (if not at the bottom).
-- Let's assume there are floors 1, 2 .. 10.

type ElevatorState = Int            -- state is given by the floor the elevator is at
data ElevatorTransition = Up | Down -- elevator can change (transition) state by either going up or down

elevatorTransit :: ElevatorState -> ElevatorTransition -> Maybe ElevatorState
elevatorTransit floor Up
    | floor < 10 = Just $ floor + 1
    | otherwise  = Nothing
elevatorTransit floor Down
    | floor > 1  = Just $ floor - 1
    | otherwise  = Nothing

elevatorInitialState :: ElevatorState
elevatorInitialState = 1

elevatorIsFinal :: ElevatorState -> Bool
elevatorIsFinal = const False

-- As another example, we could model an auction as a state machine. Let's assume the minimal bid is 100.

type AuctionState = Maybe (Int, String)

data AuctionTransition = Bid Int String

auctionInitialState :: AuctionState
auctionInitialState = Nothing

auctionTransit :: AuctionState -> AuctionTransition -> Maybe AuctionState
auctionTransit Nothing                    (Bid bid bidder)
    | bid >= 100 = Just $ Just (bid, bidder)
    | otherwise  = Nothing
auctionTransit (Just (oldBid, oldBidder)) (Bid newBid newBidder)
    | newBid > oldBid = Just $ Just (newBid, newBidder)
    | otherwise       = Nothing

auctionIsFinal :: AuctionState -> Bool
auctionIsFinal = const False           -- this is not really right - we should consider time and allow the auction to end
                                       -- when there hasn't been a new bid for a certain time

-- GOAL: Simulate state machines as Plutus Contracts
--
-- Idea:
--    state machine -- script address/validator
--    state         -- datum of "the" output at that address
--    transition    -- transaction, where the redeemer of the consuming input is the transition to be applied


--   Address: ScriptAddress sid                 Address: ScriptAddress sid
--   Datum: s                        ----       Datum s', provided transtion s t = Just s'
--                                redeemer: t
-}

data StateMachine s t = StateMachine
    { initialState :: s
    , transit      :: s -> t -> Maybe s
    , isFinal      :: s -> Bool
    }

-- How do we identify the "right" output that holds the state of our machine? There could be
-- many outputs at that script address, because there is no way to prevent somebody from creating
-- outputs at arbitrary addresses.
--
-- Idea: Create a unique token for this state machine and attach it to "the right" output.

stateMachineScript :: forall s t. (Eq s, Typeable s, Typeable t)
                   => StateMachine s t
                   -> Token
                   -> (s -> t -> Maybe (s, Output) -> Script)
                   -> Script
stateMachineScript sm token cont = Script $ \sid value datum index outputs tx -> fromEither $ do

    unless (tokenAmount token value == 1) $
        throwError "unique token is not present in the old output"

    state         <- case fromDynamic datum of
                        Nothing -> throwError "datum has the wrong type"
                        Just s  -> return (s :: s)
    transition    <- case fromDynamic $ getRedeemer index tx of
                        Nothing -> throwError "redeemer has the wrong type"
                        Just t  -> return (t :: t)
    expectedState <- case transit sm state transition of
                        Nothing -> throwError "transition not allowed in this state"
                        Just s  -> return s

    if isFinal sm expectedState
        then do
            unless (null $ relevantOutputs sid tx) $
                throwError "no output at the script address expected in final state"
            toEither $ runScript (cont state transition Nothing) sid value datum index outputs tx
        else do
            output        <- case relevantOutputs sid tx of
                                [o] -> return o
                                _   -> throwError "expected exactly one output at the script address for the state machine"
            unless (tokenAmount token (output ^. oValue) == 1) $
                throwError "unique token is not present in the new output"

            actualState   <- case fromDynamic $ output ^. oDatum of
                                Nothing -> throwError "output datum has the wrong type"
                                Just s  -> return (s :: s)
            unless (actualState == expectedState) $
                throwError "actual state of the output does not agree with the expected state"

            toEither $ runScript (cont state transition (Just (expectedState, output))) sid value datum index outputs tx

  where
    relevantOutputs :: ScriptId -> Tx -> [Output]
    relevantOutputs sid tx = [o | o <- tx ^. txOutputs, o ^. oAddress == ScriptAddress sid]

trivialCont :: s -> t -> Maybe (s, Output) -> Script
trivialCont _s _t _mso = Script $ \_sid _value _datum _index _outputs _tx -> Validated

-- | monetary policy of our unique token
uniqueTokenScript :: String      -- ^ token name
                  -> (TxId, Int) -- ^ "pointer" to an output that must be consumed during forging
                  -> Script
uniqueTokenScript tn (tid, i) = Script $ \sid _value _datum _index _outputs tx -> fromEither $ do
    let token = Token sid tn
    unless (tokenAmount token (tx ^. txForge) == 1) $
        throwError "unique token must be forged with amount one"
    unless (any (\input -> input ^. iTxId == tid &&
                           input ^. iIx   == i) $ tx ^. txInputs) $
        throwError "required output not consumed by forging transaction"

deployStateMachine :: (Eq s, Typeable s, Typeable t)
                   => StateMachine s t
                   -> (s -> t -> Maybe (s, Output) -> Script) -- ^ continuation script
                   -> PubKey                                  -- ^ "owner" of the state machine
                   -> ChainM (ScriptId, TxId, Token)
deployStateMachine sm cont owner = do

    -- create output that will be required to be consumed during forging of the unique token
    tid1 <- freshTxId
    addTx Tx
        { _txId        = tid1
        , _txInputs    = []
        , _txSignees   = [owner]
        , _txOutputs   = [ Output (PKAddress owner) mempty unit ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = mempty
        }

    uniqueTokenSid <- uploadScript $ uniqueTokenScript "STATEMACHINE" (tid1, 0)
    let token = Token uniqueTokenSid "STATEMACHINE"

    -- create output at the address of the monetary policy of our unique token
    tid2 <- freshTxId
    addTx Tx
        { _txId        = tid2
        , _txInputs    = []
        , _txSignees   = [owner]
        , _txOutputs   = [ Output (ScriptAddress uniqueTokenSid) mempty unit ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = mempty
        }

    -- forge the unique token
    tid3 <- freshTxId
    addTx Tx
        { _txId        = tid3
        , _txInputs    = [ Input tid1 0 unit
                         , Input tid2 0 unit
                         ]
        , _txSignees   = [owner]
        , _txOutputs   = [ Output (PKAddress owner) (fromToken token 1) unit ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = fromToken token 1
        }

    stateMachineSid <- uploadScript $ stateMachineScript sm token cont
    tid4 <- freshTxId
    addTx Tx
        { _txId        = tid4
        , _txInputs    = [Input tid3 0 unit]
        , _txSignees   = [owner]
        , _txOutputs   = [ Output
                            { _oAddress = ScriptAddress stateMachineSid
                            , _oValue   = fromToken token 1
                            , _oDatum   = toDyn $ initialState sm
                            }
                         ]
        , _txSlotRange = SlotRange 0 Forever
        , _txForge     = mempty
        }

    return (stateMachineSid, tid4, token)
