module Test.Automata.Regular.NFA.B where

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

runB :: (List Alphabet) -> String
runB string = unV show go bNFA
  where
    go n = if n `accepts` string' then "Yes!" else "Nope!"
    string' = Sigma <$> string

main :: Effect Unit
main = do
  print "Will the b machine accept the string 'b'?"
  print $ runB (B : Nil)

  print "Will the b machine accept the string 'bbb'?"
  print $ runB (B : B : B : Nil)

  print "Will the b machine accept the string 'a'?"
  print $ runB (A : Nil)

  print "Will the b machine accept the string 'bab'?"
  print $ runB (B : A : B : Nil)

instance showStateB :: Show StateB where
  show B0    = "B0"
  show B1    = "B1"
  show BFail = "BFail"

instance eqStateB :: Eq StateB where
  eq = eq `on` show

instance ordStateB :: Ord StateB where
  compare = compare `on` show
