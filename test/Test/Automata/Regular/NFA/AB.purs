module Test.Automata.Regular.NFA.AB (suite) where

import Prelude

import Automata.Combinators (concatenate)
import Automata.Epsilon (Epsilon(..))
import Automata.Regular.NFA (ConcatenateStates, NFA, accepts)
import Data.List (List(..), (:))
import Test.Automata.Regular.NFA.A (StateA, aNFA)
import Test.Automata.Regular.NFA.Alphabet (Alphabet(..))
import Test.Automata.Regular.NFA.B (StateB, bNFA)
import Test.Unit as Test.Unit
import Test.Unit.Assert as Test.Unit.Assert

-- | We want to describe a language that accepts strings with 1 or more `a`s
-- | followed by 1 or more `b`s.
-- | We do this by concatenating two languages together.

type ABNFA = NFA (ConcatenateStates StateA StateB) Alphabet

abNFA :: ABNFA
abNFA = concatenate aNFA bNFA

runAB :: (Boolean -> Test.Unit.Test) -> List Alphabet -> Test.Unit.Test
runAB assertion string = assertion (abNFA `accepts` map Sigma string)

suite :: Test.Unit.TestSuite
suite = Test.Unit.suite "NFA.AB" do
  Test.Unit.test
    "Will the ab machine accept the string 'a'?"
    (runAB (Test.Unit.Assert.assertFalse "It should not have!") $ A : Nil)

  Test.Unit.test
    "Will the ab machine accept the string 'b'?"
    (runAB (Test.Unit.Assert.assertFalse "It should not have!") $ B : Nil)

  Test.Unit.test
    "Will the ab machine accept the string 'ab'?"
    (runAB (Test.Unit.Assert.assert "It should have!") $ A : B : Nil)

  Test.Unit.test
    "Will the ab machine accept the string 'aabb'?"
    (runAB (Test.Unit.Assert.assert "It should have!") $ A : A : B : B : Nil)
