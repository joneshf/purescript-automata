module Examples.Automata.Regular.NFA.AB where

  import Automata.Epsilon (Epsilon(..))
  import Automata.Regular.NFA

  import Data.Function (on)
  import Data.Set (Set(), empty, fromList, singleton)
  import Data.Validation (V(), runV)

  import Debug.Trace (print)

  type ANFA = NFA StateA Alphabet

  type BNFA = NFA StateB Alphabet

  data StateA = A0 | A1 | AFail

  data StateB = B0 | B1 | BFail

  data Alphabet = A | B

  deltaA :: StateA -> Epsilon Alphabet -> Set StateA
  deltaA A0    A = singleton A1
  deltaA A1    A = singleton A1
  deltaA _     B = singleton AFail
  deltaA AFail _ = singleton AFail
  deltaA _     _ = empty

  deltaB :: StateB -> Epsilon Alphabet -> Set StateB
  deltaB B0    B = singleton B1
  deltaB B1    B = singleton B1
  deltaB _     A = singleton BFail
  deltaB BFail _ = singleton BFail
  deltaB _     _ = empty

  initialA = A0
  acceptingA = singleton A1

  initialB = B0
  acceptingB = singleton B1

  aNFA = nfa (fromList [A0, A1])
             (fromList [A, B])
             deltaA
             initialA
             acceptingA

  bNFA = nfa (fromList [B0, B1])
             (fromList [A, B])
             deltaB
             initialB
             acceptingB

  runA :: [Alphabet] -> String
  runA string = runV show go aNFA
    where
      go n = if n `accepts` string' then "Yes!" else "Nope!"
      string' = Sigma <$> string

  runB :: [Alphabet] -> String
  runB string = runV show go bNFA
    where
      go n = if n `accepts` string' then "Yes!" else "Nope!"
      string' = Sigma <$> string

  main = do
    print "Will the a machine accept the string 'a'?"
    print $ run [A]

    print "Will the a machine accept the string 'b'?"
    print $ run [B]

    print "Will the a machine accept the string 'aba'?"
    print $ run [A, B, A]

    print "Will the b machine accept the string 'b'?"
    print $ run [B]

    print "Will the b machine accept the string 'a'?"
    print $ run [A]

    print "Will the b machine accept the string 'bab'?"
    print $ run [B, A, B]

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
