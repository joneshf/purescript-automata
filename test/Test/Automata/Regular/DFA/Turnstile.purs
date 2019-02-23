module Test.Automata.Regular.DFA.Turnstile where

import Prelude

import Automata.Regular.DFA (DFA, DFAError, dfa, accepts)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Set (fromFoldable, singleton)
import Data.Validation.Semigroup (V, unV)
import Effect (Effect)
import Test.Unit.Console (print)

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

turnstile :: V (List DFAError) Turnstile
turnstile = dfa (fromFoldable [Locked, Unlocked])
                (fromFoldable [Coin, Turn])
                delta
                Locked
                (singleton Locked)

bueno :: List Input -> String
bueno inputs = unV show go turnstile
  where
    go x = if x `accepts` inputs then "Yes!" else "Nope!"

main :: Effect Unit
main = do
  print "We approach a turnstile"
  print "If we put in a coin and turn it, are we good?"
  print $ bueno (Coin : Turn : Nil)

  print "Excellent : what if we put in two coins?"
  print $ bueno (Coin : Coin : Nil)

-- Nevermind this boilerplate...
instance showInput :: Show Input where
  show Coin = "Coin"
  show Turn = "Turn"

instance showState :: Show State where
  show Locked   = "Locked"
  show Unlocked = "Unlocked"

instance eqInput :: Eq Input where
  eq = eq `on` show

instance ordInput :: Ord Input where
  compare = compare `on` show

instance eqState :: Eq State where
  eq = eq `on` show

instance ordState :: Ord State where
  compare = compare `on` show
