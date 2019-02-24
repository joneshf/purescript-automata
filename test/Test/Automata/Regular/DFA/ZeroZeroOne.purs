module Test.Automata.Regular.DFA.ZeroZeroOne (suite) where

import Prelude

import Automata.Regular.DFA (DFA, DFAError, dfa, accepts)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Set (fromFoldable, singleton)
import Data.Validation.Semigroup (V, unV)
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

-- | We want to represent a language that only accepts strings with "001".
-- | The states are {Q, Q0, Q00, Q001}
-- | The alphabet is {0, 1}
-- | The transitions are:
-- |    * Initially skip all `1`s until a `0` is seen, then go to `Q0`.
-- |    * If a `0` is seen in `Q0` go to `Q00`.
-- |    * If a `1` is seen in `Q0` go to `Q`.
-- |    * If a `0` is seen in `Q00` stay there.
-- |    * If a `1` is seen in `Q00` go to `Q001`.
-- |    * Stay in `Q001` for any input.
-- | The machine starts in the `Q` state.
-- | The machine accepts if it ends up in the `Q001` state.

type ZeroZeroOne = DFA State Alphabet

data State = Q | Q0 | Q00 | Q001

data Alphabet = Zero | One

delta :: State -> Alphabet -> State
delta Q    Zero = Q0
delta Q    One  = Q
delta Q0   Zero = Q00
delta Q0   One  = Q
delta Q00  Zero = Q00
delta Q00  One  = Q001
delta Q001 _    = Q001

zeroZeroOne :: V (List DFAError) ZeroZeroOne
zeroZeroOne = dfa (fromFoldable [Q, Q0, Q00, Q001])
                  (fromFoldable [Zero, One])
                  delta
                  Q
                  (singleton Q001)

run :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
run assertion string = unV noGo go zeroZeroOne
  where
    go x = assertion (x `accepts` string)
    noGo x = Test.Unit.failure (show x)


suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "DFA.ZeroZeroOne" do
  Test.Unit.test
    "Will the machine accept the string '0010'?"
    (run (Test.Unit.Assert.assert "It should have!") $ Zero : Zero : One : Zero : Nil)

  Test.Unit.test
    "Will the machine accept the string '1001'?"
    (run (Test.Unit.Assert.assert "It should have!") $ One : Zero : Zero : One : Nil)

  Test.Unit.test
    "Will the machine accept the string '001'?"
    (run (Test.Unit.Assert.assert "It should have!") $ Zero : Zero : One : Nil)

  Test.Unit.test
    "Will the machine accept the string '11111110011111'?"
    (run (Test.Unit.Assert.assert "It should have!") $ One : One : One : One : One : One : One : Zero : Zero : One : One : One : One : One : Nil)

-- Again, boilerplate
instance showState :: Show State where
  show Q    = "Q"
  show Q0   = "Q0"
  show Q00  = "Q00"
  show Q001 = "Q001"

instance eqState :: Eq State where
  eq = eq `on` show

instance ordState :: Ord State where
  compare = compare `on` show

instance showAlphabet :: Show Alphabet where
  show Zero  = "0"
  show One   = "1"

instance eqAlphabet :: Eq Alphabet where
  eq = eq `on` show

instance ordAlphabet :: Ord Alphabet where
  compare = compare `on` show
