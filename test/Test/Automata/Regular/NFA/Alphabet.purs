module Test.Automata.Regular.NFA.Alphabet where

import Prelude

import Data.Enum as Data.Enum
import Data.Function (on)
import Data.Generic.Rep as Data.Generic.Rep
import Data.Generic.Rep.Bounded as Data.Generic.Rep.Bounded
import Data.Generic.Rep.Enum as Data.Generic.Rep.Enum

data Alphabet = A | B

derive instance genericAlphabet :: Data.Generic.Rep.Generic Alphabet _

instance showAlphabet :: Show Alphabet where
  show A = "A"
  show B = "B"

instance eqAlphabet :: Eq Alphabet where
  eq = eq `on` show

instance ordAlphabet :: Ord Alphabet where
  compare = compare `on` show

instance boundedAlphabet :: Bounded Alphabet where
  bottom = Data.Generic.Rep.Bounded.genericBottom
  top = Data.Generic.Rep.Bounded.genericTop

instance enumAlphabet :: Data.Enum.Enum Alphabet where
  pred = Data.Generic.Rep.Enum.genericPred
  succ = Data.Generic.Rep.Enum.genericSucc

instance boundedEnumAlphabet :: Data.Enum.BoundedEnum Alphabet where
  cardinality = Data.Enum.defaultCardinality
  toEnum x = Data.Enum.toEnum x
  fromEnum x = Data.Enum.fromEnum x
