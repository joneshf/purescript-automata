module Test.Automata.Regular.DFA.Turnstile (suite) where

import Prelude

import Automata.Regular.DFA (DFA, DFAError, dfa, accepts)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Set (fromFoldable, singleton)
import Data.Validation.Semigroup (V, unV)
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

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

run :: (Boolean -> Test.Unit.Test) -> List Input -> Test.Unit.Test
run assertion string = unV noGo go turnstile
  where
    go x = assertion (x `accepts` string)
    noGo x = Test.Unit.failure (show x)

suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "DFA.Turnstile" do
  Test.Unit.suite "We approach a turnstile" do
    Test.Unit.test
      "If we put in a coin and turn it, are we good?"
      (run (Test.Unit.Assert.assert "We should be!") $ Coin : Turn : Nil)

    Test.Unit.test
      "Excellent : what if we put in two coins?"
      (run (Test.Unit.Assert.assertFalse "We shouldn't be!") $ Coin : Coin : Nil)

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
