module Examples.Automata.Regular.NFA.A where

  import Automata.Epsilon (Epsilon(..))
  import Automata.Regular.NFA

  import Data.Function (on)
  import Data.Set (Set(), empty, fromList, singleton)
  import Data.Validation (V(), runV)

  import Debug.Trace (print)

  import Examples.Automata.Regular.NFA.Alphabet (Alphabet(..))

  -- | We want to describe a language that accepts strings with 1 or more `a`.

  type ANFA = NFA StateA Alphabet

  data StateA = A0 | A1 | AFail

  deltaA :: StateA -> Epsilon Alphabet -> Set StateA
  deltaA A0    (Sigma A) = singleton A1
  deltaA A1    (Sigma A) = singleton A1
  deltaA _     (Sigma B) = singleton AFail
  deltaA AFail _         = singleton AFail
  deltaA _     _         = empty

  initialA = A0
  acceptingA = singleton A1

  aNFA :: V [NFAError] ANFA
  aNFA = nfa (fromList [A0, A1, AFail])
             (fromList [A, B])
             deltaA
             initialA
             acceptingA

  runA :: [Alphabet] -> String
  runA string = runV show go aNFA
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
