module Test.Automata.Regular.DFA.OddOnes (suite) where

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

-- | We want to represent a language that has only an odd number of 1's in it.
-- | The states are {Odd, Even}.
-- |    Meaning there have been a odd number of 1's so far
-- |    or an even number of 1's so far, respectively.
-- | The alphabet is the decimal integers, i.e. {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
-- | The transitions are:
-- |    * For each 1 seen, alternate the state.
-- |    * For any other letter, keep the same state.
-- | The machine starts in the `Even` state.
-- | The machine accepts if it ends up in the `Odd` state.

type OddOnes = DFA State Alphabet

data State = Odd | Even

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

data Alphabet = Zero
              | One
              | Two
              | Three
              | Four
              | Five
              | Six
              | Seven
              | Eight
              | Nine

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
delta Odd  One = Even
delta Even One = Odd
delta s    _   = s

oddOnes :: OddOnes
oddOnes = dfa delta Even (Data.Set.singleton Odd)

run :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
run assertion string = assertion (oddOnes `accepts` string)

suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "DFA.OddOnes" do
  Test.Unit.test
    "Will the machine accept the string '123'?"
    (run (Test.Unit.Assert.assert "It should have!") $ One : Two : Three : Nil)

  Test.Unit.test
    "Will the machine accept the string '0'?"
    (run (Test.Unit.Assert.assertFalse "It should not have!") $ Zero : Nil)

  Test.Unit.test
    "Will the machine accept the string '11'?"
    (run (Test.Unit.Assert.assertFalse "It should not have!") $ One : One : Nil)
