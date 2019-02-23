module Test.Automata.Regular.NFA.OneThirdFromEnd where

import Prelude

import Automata.Epsilon (Epsilon(..))
import Automata.Regular.NFA (NFA, NFAError, accepts, nfa)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Set (Set, empty, fromFoldable, singleton)
import Data.Validation.Semigroup (V, unV)
import Effect (Effect)
import Test.Unit.Console (print)

type OneThirdFromEnd = NFA State Alphabet

data State = Q1 | Q2 | Q3 | Q4

data Alphabet = Zero | One

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

oneThirdFromEnd :: V (List NFAError) (NFA State Alphabet)
oneThirdFromEnd = nfa (fromFoldable [Q1, Q2, Q3, Q4])
                      (fromFoldable [Zero, One])
                      delta
                      initial
                      accepting

run :: List Alphabet -> String
run string = unV show go oneThirdFromEnd
  where
    go n = if n `accepts` string' then "Yes!" else "Nope!"
    string' = Sigma <$> string

main :: Effect Unit
main = do
  print "Will the machine accept the string '000100'?"
  print $ run (Zero : Zero : Zero : One : Zero : Zero : Nil)

  print "Will the machine accept the string '0011'?"
  print $ run (Zero : Zero : One : One : Nil)

-- Again, boilerplate
instance showState :: Show State where
  show Q1 = "Q1"
  show Q2 = "Q2"
  show Q3 = "Q3"
  show Q4 = "Q4"

instance eqState :: Eq State where
  eq = eq `on` show

instance ordState :: Ord State where
  compare = compare `on` show

instance showAlphabet :: Show Alphabet where
  show Zero  = "0"
  show One   = "1"

instance eqAlphabet :: Eq Alphabet where
  eq = eq `on` show

instance ordAlphabet :: Ord Alphabet where
  compare = compare `on` show
