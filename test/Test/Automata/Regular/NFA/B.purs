module Test.Automata.Regular.NFA.B (StateB, bNFA, suite) where

import Prelude

import Automata.Epsilon (Epsilon(..))
import Automata.Regular.NFA (NFA, accepts, nfa)
import Data.Enum as Data.Enum
import Data.Generic.Rep as Data.Generic.Rep
import Data.Generic.Rep.Bounded as Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum as Data.Generic.Rep.Enum
import Data.List (List(..), (:))
import Data.Set (Set, empty, singleton)
import Test.Automata.Regular.NFA.Alphabet (Alphabet(..))
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

-- | We want to describe a language that accepts strings with 1 or more `b`.

type BNFA = NFA StateB Alphabet

data StateB = B0 | B1 | BFail

derive instance genericStateB :: Data.Generic.Rep.Generic StateB _

derive instance eqStateB :: Eq StateB

derive instance ordStateB :: Ord StateB

instance boundedStateB :: Bounded StateB where
  bottom = Data.Generic.Rep.Bounded.genericBottom
  top = Data.Generic.Rep.Bounded.genericTop

instance enumStateB :: Data.Enum.Enum StateB where
  pred = Data.Generic.Rep.Enum.genericPred
  succ = Data.Generic.Rep.Enum.genericSucc

instance boundedEnumStateB :: Data.Enum.BoundedEnum StateB where
  cardinality = Data.Enum.defaultCardinality
  toEnum x = Data.Enum.toEnum x
  fromEnum x = Data.Enum.fromEnum x

deltaB :: StateB -> Epsilon Alphabet -> Set StateB
deltaB B0    (Sigma B) = singleton B1
deltaB B1    (Sigma B) = singleton B1
deltaB _     (Sigma A) = singleton BFail
deltaB BFail _         = singleton BFail
deltaB _     _         = empty

initialB :: StateB
initialB = B0
acceptingB :: Set StateB
acceptingB = singleton B1

bNFA :: BNFA
bNFA = nfa deltaB initialB acceptingB

runB :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
runB assertion string = assertion (bNFA `accepts` map Sigma string)

suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "NFA.B" do
  Test.Unit.test
    "Will the b machine accept the string 'b'?"
    (runB (Test.Unit.Assert.assert "It should have!") $ B : Nil)

  Test.Unit.test
    "Will the b machine accept the string 'bbb'?"
    (runB (Test.Unit.Assert.assert "It should have!") $ B : B : B : Nil)

  Test.Unit.test
    "Will the b machine accept the string 'a'?"
    (runB (Test.Unit.Assert.assertFalse "It should not have!") $ A : Nil)

  Test.Unit.test
    "Will the b machine accept the string 'bab'?"
    (runB (Test.Unit.Assert.assertFalse "It should not have!") $ B : A : B : Nil)
