module Test.Automata.Regular.DFA.ZeroZeroOne (suite) where

import Prelude

import Automata.Regular.DFA (DFA, dfa, accepts)
import Data.Enum as Data.Enum
import Data.Generic.Rep as Data.Generic.Rep
import Data.Generic.Rep.Bounded as Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum as Data.Generic.Rep.Enum
import Data.List (List(..), (:))
import Data.Set as Data.Set
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

derive instance genericState :: Data.Generic.Rep.Generic State _

derive instance eqState :: Eq State

derive instance ordState :: Ord State

instance boundedState :: Bounded State where
  bottom = Data.Generic.Rep.Bounded.genericBottom
  top = Data.Generic.Rep.Bounded.genericTop

instance enumState :: Data.Enum.Enum State where
  pred = Data.Generic.Rep.Enum.genericPred
  succ = Data.Generic.Rep.Enum.genericSucc

instance boundedEnumState :: Data.Enum.BoundedEnum State where
  cardinality = Data.Enum.defaultCardinality
  toEnum x = Data.Enum.toEnum x
  fromEnum x = Data.Enum.fromEnum x

data Alphabet = Zero | One

derive instance genericAlphabet :: Data.Generic.Rep.Generic Alphabet _

derive instance eqAlphabet :: Eq Alphabet

derive instance ordAlphabet :: Ord Alphabet

instance boundedAlphabet :: Bounded Alphabet where
  bottom = Data.Generic.Rep.Bounded.genericBottom
  top = Data.Generic.Rep.Bounded.genericTop

instance enumAlphabet :: Data.Enum.Enum Alphabet where
  pred = Data.Generic.Rep.Enum.genericPred
  succ = Data.Generic.Rep.Enum.genericSucc

instance boundedEnumAlphabet :: Data.Enum.BoundedEnum Alphabet where
  cardinality = Data.Enum.defaultCardinality
  toEnum x = Data.Enum.toEnum x
  fromEnum x = Data.Enum.fromEnum x

delta :: State -> Alphabet -> State
delta Q    Zero = Q0
delta Q    One  = Q
delta Q0   Zero = Q00
delta Q0   One  = Q
delta Q00  Zero = Q00
delta Q00  One  = Q001
delta Q001 _    = Q001

zeroZeroOne :: ZeroZeroOne
zeroZeroOne = dfa delta Q (Data.Set.singleton Q001)

run :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
run assertion string = assertion (zeroZeroOne `accepts` string)

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
