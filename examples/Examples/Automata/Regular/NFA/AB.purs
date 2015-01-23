module Examples.Automata.Regular.NFA.AB where

  import Automata.Combinators (concatenate)
  import Automata.Epsilon (Epsilon(..))
  import Automata.Regular.NFA (ConcatenateStates(), NFA(), NFAError(), accepts)

  import Data.Validation (V(), runV)

  import Debug.Trace (print)

  import Examples.Automata.Regular.NFA.Alphabet (Alphabet(..))
  import Examples.Automata.Regular.NFA.A (StateA(), aNFA)
  import Examples.Automata.Regular.NFA.B (StateB(), bNFA)

  -- | We want to describe a language that accepts strings with 1 or more `a`s
  -- | followed by 1 or more `b`s.
  -- | We do this by concatenating two languages together.

  type ABNFA = NFA (ConcatenateStates StateA StateB) Alphabet

  abNFA :: V [NFAError] ABNFA
  abNFA = concatenate <$> aNFA <*> bNFA

  runAB :: [Alphabet] -> String
  runAB string = runV show go abNFA
    where
      go n = if n `accepts` string' then "Yes!" else "Nope!"
      string' = Sigma <$> string

  main = do
    print "Will the ab machine accept the string 'a'?"
    print $ runAB [A]

    print "Will the ab machine accept the string 'b'?"
    print $ runAB [B]

    print "Will the ab machine accept the string 'ab'?"
    print $ runAB [A, B]

    print "Will the ab machine accept the string 'aabb'?"
    print $ runAB [A, A, B, B]
