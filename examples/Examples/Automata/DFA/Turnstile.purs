module Examples.Automata.DFA.Turnstile where

  import Automata.DFA (DFA(), DFAError(), dfa, accepts)

  import Data.Function (on)
  import Data.Set (fromList, singleton)
  import Data.Validation (V(), runV)

  import Debug.Trace (print)

  -- | We want to represent a turnstile "language".
  -- | The states are {Locked, Unlocked}.
  -- | The alphabet is {Coin, Turn}.
  -- | The transitions are:
  -- |    * Inserting a `Coin` into a `Locked` turnstile unlocks it.
  -- |    * Turning an `Unlocked` turnstile locks it again.
  -- |    * Anything else keeps the turnstile in the current state.
  -- | The turnstile starts in the locked state.
  -- | The turnstile is good if it ends up in a locked state again.

  type Turnstile = DFA State Input

  data Input = Coin | Turn

  data State = Locked | Unlocked

  delta :: State -> Input -> State
  delta Locked   Coin = Unlocked
  delta Locked   Turn = Locked
  delta Unlocked Coin = Unlocked
  delta Unlocked Turn = Locked

  turnstile :: V [DFAError] Turnstile
  turnstile = dfa (fromList [Locked, Unlocked])
                  (fromList [Coin, Turn])
                  delta
                  Locked
                  (singleton Locked)

  bueno :: [Input] -> String
  bueno inputs = runV show go turnstile
    where
      go turnstile = if turnstile `accepts` inputs then "Yes!" else "Nope!"

  main = do
    print "We approach a turnstile"
    print "If we put in a coin and turn it, are we good?"
    print $ bueno [Coin, Turn]

    print "Excellent, what if we put in two coins?"
    print $ bueno [Coin, Coin]

  -- Nevermind this boilerplate...
  instance showInput :: Show Input where
    show Coin = "Coin"
    show Turn = "Turn"

  instance showState :: Show State where
    show Locked   = "Locked"
    show Unlocked = "Unlocked"

  instance eqInput :: Eq Input where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordInput :: Ord Input where
    compare = compare `on` show

  instance eqState :: Eq State where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordState :: Ord State where
    compare = compare `on` show
