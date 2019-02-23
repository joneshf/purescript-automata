module Test.Automata.Regular.DFA.OddOnes where

import Prelude

import Automata.Regular.DFA (DFA, DFAError, dfa, accepts)
import Data.Function (on)
import Data.List (List(..), (:))
import Data.Set (fromFoldable, singleton)
import Data.Validation.Semigroup (V, unV)
import Effect (Effect)
import Test.Unit.Console (print)

-- | We want to represent a language that has only an odd number of 1's in it.
-- | The states are {Odd, Even}.
-- |    Meaning there have been a odd number of 1's so far
-- |    or an even number of 1's so far, respectively.
-- | The alphabet is the decimal integers, i.e. {0, 1, 2, 3, 4, 5, 6, 7, 8, 9}
-- | The transitions are:
-- |    * For each 1 seen, alternate the state.
-- |    * For any other letter, keep the same state.
-- | The machine starts in the `Even` state.
-- | The machine accepts if it ends up in the `Odd` state.

type OddOnes = DFA State Alphabet

data State = Odd | Even

data Alphabet = Zero
              | One
              | Two
              | Three
              | Four
              | Five
              | Six
              | Seven
              | Eight
              | Nine

delta :: State -> Alphabet -> State
delta Odd  One = Even
delta Even One = Odd
delta s    _   = s

oddOnes :: V (List DFAError) OddOnes
oddOnes = dfa (fromFoldable [Odd, Even])
              (fromFoldable [Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine])
              delta
              Even
              (singleton Odd)

run :: List Alphabet -> String
run string = unV show go oddOnes
  where
    go x = if x `accepts` string then "Yes!" else "Nope!"

main :: Effect Unit
main = do
  print "Will the machine accept the string '123'?"
  print $ run (One : Two : Three : Nil)

  print "Will the machine accept the string '0'?"
  print $ run (Zero : Nil)

  print "Will the machine accept the string '11'?"
  print $ run (One : One : Nil)

-- Again, boilerplate
instance showState :: Show State where
  show Odd  = "Odd"
  show Even = "Even"

instance eqState :: Eq State where
  eq = eq `on` show

instance ordState :: Ord State where
  compare = compare `on` show

instance showAlphabet :: Show Alphabet where
  show Zero  = "0"
  show One   = "1"
  show Two   = "2"
  show Three = "3"
  show Four  = "4"
  show Five  = "5"
  show Six   = "6"
  show Seven = "7"
  show Eight = "8"
  show Nine  = "9"

instance eqAlphabet :: Eq Alphabet where
  eq = eq `on` show

instance ordAlphabet :: Ord Alphabet where
  compare = compare `on` show
