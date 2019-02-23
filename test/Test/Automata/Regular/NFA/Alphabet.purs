module Test.Automata.Regular.NFA.Alphabet where

import Prelude

import Data.Function (on)

data Alphabet = A | B

instance showAlphabet :: Show Alphabet where
  show A = "A"
  show B = "B"

instance eqAlphabet :: Eq Alphabet where
  eq = eq `on` show

instance ordAlphabet :: Ord Alphabet where
  compare = compare `on` show
