module Examples.Automata.Regular.NFA.Alphabet where

  import Data.Function (on)

  data Alphabet = A | B

  instance showAlphabet :: Show Alphabet where
    show A = "A"
    show B = "B"

  instance eqAlphabet :: Eq Alphabet where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordAlphabet :: Ord Alphabet where
    compare = compare `on` show
