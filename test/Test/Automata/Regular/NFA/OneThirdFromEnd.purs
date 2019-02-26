module Test.Automata.Regular.NFA.OneThirdFromEnd (suite) where

import Prelude

import Automata.Epsilon (Epsilon(..))
import Automata.Regular.NFA (NFA, accepts, nfa)
import Data.Enum as Data.Enum
import Data.Generic.Rep as Data.Generic.Rep
import Data.Generic.Rep.Bounded as Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum as Data.Generic.Rep.Enum
import Data.List (List(..), (:))
import Data.Set (Set, empty, fromFoldable, singleton)
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

type OneThirdFromEnd = NFA State Alphabet

data State = Q1 | Q2 | Q3 | Q4

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

delta :: State -> Epsilon Alphabet -> Set State
delta Q1 (Sigma Zero) = singleton Q1
delta Q1 (Sigma One)  = fromFoldable [Q1, Q2]
delta Q2 (Sigma Zero) = singleton Q3
delta Q2 (Sigma One)  = singleton Q3
delta Q3 (Sigma Zero) = singleton Q4
delta Q3 (Sigma One)  = singleton Q4
delta _  _            = empty

initial :: State
initial = Q1
accepting :: Set State
accepting = singleton Q4

oneThirdFromEnd :: NFA State Alphabet
oneThirdFromEnd = nfa delta initial accepting

run :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
run assertion string = assertion (oneThirdFromEnd `accepts` map Sigma string)

suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "NFA.OneThirdFromEnd" do
  Test.Unit.test
    "Will the machine accept the string '000100'?"
    (run (Test.Unit.Assert.assert "It should have!") $ Zero : Zero : Zero : One : Zero : Zero : Nil)

  Test.Unit.test
    "Will the machine accept the string '0011'?"
    (run (Test.Unit.Assert.assertFalse "It should not have!") $ Zero : Zero : One : One : Nil)
