module Examples.Automata.Regular.NFA.AB where

  import Automata.Combinators (concatenate)
  import Automata.Epsilon (Epsilon(..))
  import Automata.Regular.NFA

  import Data.Function (on)
  import Data.Set (Set(), empty, fromList, singleton)
  import Data.Validation (V(), runV)

  import Debug.Trace (print)

  type ANFA = NFA StateA Alphabet

  type BNFA = NFA StateB Alphabet

  type ABNFA = NFA (ConcatenateStates StateA StateB) Alphabet

  data StateA = A0 | A1 | AFail

  data StateB = B0 | B1 | BFail

  data Alphabet = A | B

  deltaA :: StateA -> Epsilon Alphabet -> Set StateA
  deltaA A0    (Sigma A) = singleton A1
  deltaA A1    (Sigma A) = singleton A1
  deltaA _     (Sigma B) = singleton AFail
  deltaA AFail _         = singleton AFail
  deltaA _     _         = empty

  deltaB :: StateB -> Epsilon Alphabet -> Set StateB
  deltaB B0    (Sigma B) = singleton B1
  deltaB B1    (Sigma B) = singleton B1
  deltaB _     (Sigma A) = singleton BFail
  deltaB BFail _         = singleton BFail
  deltaB _     _         = empty

  initialA = A0
  acceptingA = singleton A1

  initialB = B0
  acceptingB = singleton B1

  aNFA = nfa (fromList [A0, A1, AFail])
             (fromList [A, B])
             deltaA
             initialA
             acceptingA

  bNFA = nfa (fromList [B0, B1, BFail])
             (fromList [A, B])
             deltaB
             initialB
             acceptingB

  abNFA :: V [NFAError] ABNFA
  abNFA = concatenate <$> aNFA <*> bNFA

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

  runAB :: [Alphabet] -> String
  runAB string = runV show go abNFA
    where
      go n = if n `accepts` string' then "Yes!" else "Nope!"
      string' = Sigma <$> string

  main = do
    print "Will the a machine accept the string 'a'?"
    print $ runA [A]

    print "Will the a machine accept the string 'aaa'?"
    print $ runA [A, A, A]

    print "Will the a machine accept the string 'b'?"
    print $ runA [B]

    print "Will the a machine accept the string 'aba'?"
    print $ runA [A, B, A]

    print "Will the b machine accept the string 'b'?"
    print $ runB [B]

    print "Will the b machine accept the string 'bbb'?"
    print $ runB [B, B, B]

    print "Will the b machine accept the string 'a'?"
    print $ runB [A]

    print "Will the b machine accept the string 'bab'?"
    print $ runB [B, A, B]

    print "Will the ab machine accept the string 'a'?"
    print $ runAB [A]

    print "Will the ab machine accept the string 'b'?"
    print $ runAB [B]

    print "Will the ab machine accept the string 'ab'?"
    print $ runAB [A, B]

    print "Will the ab machine accept the string 'aabb'?"
    print $ runAB [A, A, B, B]

  -- Again, boilerplate
  instance showStateA :: Show StateA where
    show A0    = "A0"
    show A1    = "A1"
    show AFail = "AFail"

  instance eqStateA :: Eq StateA where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordStateA :: Ord StateA where
    compare = compare `on` show

  instance showStateB :: Show StateB where
    show B0    = "B0"
    show B1    = "B1"
    show BFail = "BFail"

  instance eqStateB :: Eq StateB where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordStateB :: Ord StateB where
    compare = compare `on` show

  instance showAlphabet :: Show Alphabet where
    show A = "A"
    show B = "B"

  instance eqAlphabet :: Eq Alphabet where
    (==) = (==) `on` show
    (/=) i i' = not (i == i')

  instance ordAlphabet :: Ord Alphabet where
    compare = compare `on` show
