module Test.Automata.Regular.NFA.A where

import Prelude

import Automata.Epsilon (Epsilon(..))
import Automata.Regular.NFA (NFA, NFAError, accepts, nfa)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Set (Set, empty, fromFoldable, singleton)
import Data.Validation.Semigroup (V, unV)
import Effect (Effect)
import Test.Automata.Regular.NFA.Alphabet (Alphabet(..))
import Test.Unit.Console (print)

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

runA :: List Alphabet -> String
runA string = unV show go aNFA
  where
    go n = if n `accepts` string' then "Yes!" else "Nope!"
    string' = Sigma <$> string

main :: Effect Unit
main = do
  print "Will the a machine accept the string 'a'?"
  print $ runA (A : Nil)

  print "Will the a machine accept the string 'aaa'?"
  print $ runA (A : A : A : Nil)

  print "Will the a machine accept the string 'b'?"
  print $ runA (B : Nil)

  print "Will the a machine accept the string 'aba'?"
  print $ runA (A : B : A : Nil)

-- Again, boilerplate
instance showStateA :: Show StateA where
  show A0    = "A0"
  show A1    = "A1"
  show AFail = "AFail"

instance eqStateA :: Eq StateA where
  eq = eq `on` show

instance ordStateA :: Ord StateA where
  compare = compare `on` show
