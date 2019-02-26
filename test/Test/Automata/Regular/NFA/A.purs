module Test.Automata.Regular.NFA.A (StateA, aNFA, suite) where

import Prelude

import Automata.Epsilon (Epsilon(..))
import Automata.Regular.NFA (NFA, accepts, nfa)
import Data.Enum as Data.Enum
import Data.Function (on)
import Data.Generic.Rep as Data.Generic.Rep
import Data.Generic.Rep.Bounded as Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum as Data.Generic.Rep.Enum
import Data.List (List(..), (:))
import Data.Set (Set, empty, singleton)
import Test.Automata.Regular.NFA.Alphabet (Alphabet(..))
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

-- | We want to describe a language that accepts strings with 1 or more `a`.

type ANFA = NFA StateA Alphabet

data StateA = A0 | A1 | AFail

derive instance genericStateA :: Data.Generic.Rep.Generic StateA _

instance boundedStateA :: Bounded StateA where
  bottom = Data.Generic.Rep.Bounded.genericBottom
  top = Data.Generic.Rep.Bounded.genericTop

instance enumStateA :: Data.Enum.Enum StateA where
  pred = Data.Generic.Rep.Enum.genericPred
  succ = Data.Generic.Rep.Enum.genericSucc

instance boundedEnumStateA :: Data.Enum.BoundedEnum StateA where
  cardinality = Data.Enum.defaultCardinality
  toEnum x = Data.Enum.toEnum x
  fromEnum x = Data.Enum.fromEnum x

deltaA :: StateA -> Epsilon Alphabet -> Set StateA
deltaA A0    (Sigma A) = singleton A1
deltaA A1    (Sigma A) = singleton A1
deltaA _     (Sigma B) = singleton AFail
deltaA AFail _         = singleton AFail
deltaA _     _         = empty

initialA :: StateA
initialA = A0
acceptingA :: Set StateA
acceptingA = singleton A1

aNFA :: ANFA
aNFA = nfa deltaA initialA acceptingA

runA :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
runA assertion string = assertion (aNFA `accepts` map Sigma string)

suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "NFA.A" do
  Test.Unit.test
    "Will the a machine accept the string 'a'?"
    (runA (Test.Unit.Assert.assert "It should have!") $ A : Nil)

  Test.Unit.test
    "Will the a machine accept the string 'aaa'?"
    (runA (Test.Unit.Assert.assert "It should have!") $ A : A : A : Nil)

  Test.Unit.test
    "Will the a machine accept the string 'b'?"
    (runA (Test.Unit.Assert.assertFalse "It should not have!") $ B : Nil)

  Test.Unit.test
    "Will the a machine accept the string 'aba'?"
    (runA (Test.Unit.Assert.assertFalse "It should not have!") $ A : B : A : Nil)

-- Again, boilerplate
instance showStateA :: Show StateA where
  show A0    = "A0"
  show A1    = "A1"
  show AFail = "AFail"

instance eqStateA :: Eq StateA where
  eq = eq `on` show

instance ordStateA :: Ord StateA where
  compare = compare `on` show
