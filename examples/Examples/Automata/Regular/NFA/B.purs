module Examples.Automata.Regular.NFA.B where

  import Automata.Epsilon (Epsilon(..))
  import Automata.Regular.NFA (NFA(), NFAError(), accepts, nfa)

  import Data.Function (on)
  import Data.Set (Set(), empty, fromList, singleton)
  import Data.Validation (V(), runV)

  import Debug.Trace (print)

  import Examples.Automata.Regular.NFA.Alphabet (Alphabet(..))

  -- | We want to describe a language that accepts strings with 1 or more `b`.

  type BNFA = NFA StateB Alphabet

  data StateB = B0 | B1 | BFail

  deltaB :: StateB -> Epsilon Alphabet -> Set StateB
  deltaB B0    (Sigma B) = singleton B1
  deltaB B1    (Sigma B) = singleton B1
  deltaB _     (Sigma A) = singleton BFail
  deltaB BFail _         = singleton BFail
  deltaB _     _         = empty

  initialB = B0
  acceptingB = singleton B1

  bNFA :: V [NFAError] BNFA
  bNFA = nfa (fromList [B0, B1, BFail])
             (fromList [A, B])
             deltaB
             initialB
             acceptingB

  runB :: [Alphabet] -> String
  runB string = runV show go bNFA
    where
      go n = if n `accepts` string' then "Yes!" else "Nope!"
      string' = Sigma <$> string

  main = do
    print "Will the b machine accept the string 'b'?"
    print $ runB [B]

    print "Will the b machine accept the string 'bbb'?"
    print $ runB [B, B, B]

    print "Will the b machine accept the string 'a'?"
    print $ runB [A]

    print "Will the b machine accept the string 'bab'?"
    print $ runB [B, A, B]

  instance showStateB :: Show StateB where
    show B0    = "B0"
    show B1    = "B1"
    show BFail = "BFail"

  instance eqStateB :: Eq StateB where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordStateB :: Ord StateB where
    compare = compare `on` show
