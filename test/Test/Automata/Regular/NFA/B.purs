module Test.Automata.Regular.NFA.B (StateB, bNFA, suite) where

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

-- | We want to describe a language that accepts strings with 1 or more `b`.

type BNFA = NFA StateB Alphabet

data StateB = B0 | B1 | BFail

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

bNFA :: V (List NFAError) BNFA
bNFA = nfa (fromFoldable [B0, B1, BFail])
           (fromFoldable [A, B])
           deltaB
           initialB
           acceptingB

runB :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
runB assertion string = unV noGo go bNFA
  where
    go n = assertion (n `accepts` string')
    string' = Sigma <$> string
    noGo x = Test.Unit.failure (show x)

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

instance showStateB :: Show StateB where
  show B0    = "B0"
  show B1    = "B1"
  show BFail = "BFail"

instance eqStateB :: Eq StateB where
  eq = eq `on` show

instance ordStateB :: Ord StateB where
  compare = compare `on` show
