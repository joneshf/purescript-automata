module Examples.Automata.Regular.NFA.OneThirdFromEnd where

  import Automata.Epsilon (Epsilon(..))
  import Automata.Regular.NFA

  import Data.Function (on)
  import Data.Set (Set(), empty, fromList, singleton)
  import Data.Validation (V(), runV)

  import Debug.Trace (print)

  type OneThirdFromEnd = NFA State Alphabet

  data State = Q1 | Q2 | Q3 | Q4

  data Alphabet = Zero | One

  delta :: State -> Epsilon Alphabet -> Set State
  delta Q1 (Sigma Zero) = singleton Q1
  delta Q1 (Sigma One)  = fromList [Q1, Q2]
  delta Q2 (Sigma Zero) = singleton Q3
  delta Q2 (Sigma One)  = singleton Q3
  delta Q3 (Sigma Zero) = singleton Q4
  delta Q3 (Sigma One)  = singleton Q4
  delta _  _            = empty

  initial = Q1
  accepting = singleton Q4

  oneThirdFromEnd = nfa (fromList [Q1, Q2, Q3, Q4])
                        (fromList [Zero, One])
                        delta
                        initial
                        accepting

  run :: [Alphabet] -> String
  run string = runV show go oneThirdFromEnd
    where
      go n = if n `accepts` string' then "Yes!" else "Nope!"
      string' = Sigma <$> string

  main = do
    print "Will the machine accept the string '000100'?"
    print $ run [Zero, Zero, Zero, One, Zero, Zero]

    print "Will the machine accept the string '0011'?"
    print $ run [Zero, Zero, One, One]

  -- Again, boilerplate
  instance showState :: Show State where
    show Q1 = "Q1"
    show Q2 = "Q2"
    show Q3 = "Q3"
    show Q4 = "Q4"

  instance eqState :: Eq State where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordState :: Ord State where
    compare = compare `on` show

  instance showAlphabet :: Show Alphabet where
    show Zero  = "0"
    show One   = "1"

  instance eqAlphabet :: Eq Alphabet where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordAlphabet :: Ord Alphabet where
    compare = compare `on` show
