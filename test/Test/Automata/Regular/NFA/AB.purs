module Test.Automata.Regular.NFA.AB where

import Prelude

import Automata.Combinators (concatenate)
import Automata.Epsilon (Epsilon(..))
import Automata.Regular.NFA (ConcatenateStates, NFA, NFAError, accepts)
import Data.List (List(..), (:))
import Data.Validation.Semigroup (V, unV)
import Effect (Effect)
import Test.Automata.Regular.NFA.A (StateA, aNFA)
import Test.Automata.Regular.NFA.Alphabet (Alphabet(..))
import Test.Automata.Regular.NFA.B (StateB, bNFA)
import Test.Unit.Console (print)

-- | We want to describe a language that accepts strings with 1 or more `a`s
-- | followed by 1 or more `b`s.
-- | We do this by concatenating two languages together.

type ABNFA = NFA (ConcatenateStates StateA StateB) Alphabet

abNFA :: V (List NFAError) ABNFA
abNFA = concatenate <$> aNFA <*> bNFA

runAB :: List Alphabet -> String
runAB string = unV show go abNFA
  where
    go n = if n `accepts` string' then "Yes!" else "Nope!"
    string' = Sigma <$> string

main :: Effect Unit
main = do
  print "Will the ab machine accept the string 'a'?"
  print $ runAB (A : Nil)

  print "Will the ab machine accept the string 'b'?"
  print $ runAB (B : Nil)

  print "Will the ab machine accept the string 'ab'?"
  print $ runAB (A : B : Nil)

  print "Will the ab machine accept the string 'aabb'?"
  print $ runAB (A : A : B : B : Nil)
