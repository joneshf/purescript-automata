module Examples.Automata.Regular.DFA.ZeroZeroOne where

  import Automata.Regular.DFA (DFA(), DFAError(), dfa, accepts)

  import Data.Function (on)
  import Data.Set (fromList, singleton)
  import Data.Validation (V(), runV)

  import Debug.Trace (print)

  -- | We want to represent a language that only accepts strings with "001".
  -- | The states are {Q, Q0, Q00, Q001}
  -- | The alphabet is {0, 1}
  -- | The transitions are:
  -- |    * Initially skip all `1`s until a `0` is seen, then go to `Q0`.
  -- |    * If a `0` is seen in `Q0` go to `Q00`.
  -- |    * If a `1` is seen in `Q0` go to `Q`.
  -- |    * If a `0` is seen in `Q00` stay there.
  -- |    * If a `1` is seen in `Q00` go to `Q001`.
  -- |    * Stay in `Q001` for any input.
  -- | The machine starts in the `Q` state.
  -- | The machine accepts if it ends up in the `Q001` state.

  type ZeroZeroOne = DFA State Alphabet

  data State = Q | Q0 | Q00 | Q001

  data Alphabet = Zero | One

  delta :: State -> Alphabet -> State
  delta Q    Zero = Q0
  delta Q    One  = Q
  delta Q0   Zero = Q00
  delta Q0   One  = Q
  delta Q00  Zero = Q00
  delta Q00  One  = Q001
  delta Q001 _    = Q001

  zeroZeroOne :: V [DFAError] ZeroZeroOne
  zeroZeroOne = dfa (fromList [Q, Q0, Q00, Q001])
                    (fromList [Zero, One])
                    delta
                    Q
                    (singleton Q001)

  run :: [Alphabet] -> String
  run string = runV show go zeroZeroOne
    where
      go zeroZeroOne = if zeroZeroOne `accepts` string then "Yes!" else "Nope!"

  main = do
    print "Will the machine accept the string '0010'?"
    print $ run [Zero, Zero, One, Zero]

    print "Will the machine accept the string '1001'?"
    print $ run [One, Zero, Zero, One]

    print "Will the machine accept the string '001'?"
    print $ run [Zero, Zero, One]

    print "Will the machine accept the string '11111110011111'?"
    print $ run [One, One, One, One, One, One, One, Zero, Zero, One, One, One, One, One]

  -- Again, boilerplate
  instance showState :: Show State where
    show Q    = "Q"
    show Q0   = "Q0"
    show Q00  = "Q00"
    show Q001 = "Q001"

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
