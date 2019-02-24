module Test.Automata.Regular.NFA.A (StateA, aNFA, suite) where

import Prelude

import Automata.Epsilon (Epsilon(..))
import Automata.Regular.NFA (NFA, NFAError, accepts, nfa)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Set (Set, empty, fromFoldable, singleton)
import Data.Validation.Semigroup (V, unV)
import Test.Automata.Regular.NFA.Alphabet (Alphabet(..))
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

-- | We want to describe a language that accepts strings with 1 or more `a`.

type ANFA = NFA StateA Alphabet

data StateA = A0 | A1 | AFail

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

aNFA :: V (List NFAError) ANFA
aNFA = nfa (fromFoldable [A0, A1, AFail])
           (fromFoldable [A, B])
           deltaA
           initialA
           acceptingA

runA :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
runA assertion string = unV noGo go aNFA
  where
    go n = assertion (n `accepts` string')
    string' = Sigma <$> string
    noGo x = Test.Unit.failure (show x)

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
