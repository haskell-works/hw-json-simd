module HaskellWorks.Data.Json.Simd.Internal.Fsm where

import HaskellWorks.Data.Json.Simd.Internal.Fsm.Core (State (..), StateMachine)

import qualified HaskellWorks.Data.Json.Simd.Internal.Fsm.Core as FSM

{-| `StateMachine` for matching well-formed C-style comments
    The match succeeds if the state `S00` transitions to state `S00`
-}
cStyleComments :: StateMachine
cStyleComments = FSM.buildStateMachine f
  where
    -- 47 is the ASCII encoding for '/'
    -- 42 is the ASCII encoding for '*'

    f 47 S00 = S01  -- Possible  comment start: Go to state #1
    f 42 S01 = S02  -- Confirmed comment start: Go to state #2
    f 42 S02 = S03  -- Possible  comment end  : Go to state #3
    f 47 S03 = S00  -- Confirmed comment end  : Go to state #0

    f 47 S01 = S01  -- Still might be a comment start: Stay on   state #1
    f  _ S01 = S00  -- Not a comment after all       : Return to state #0

    f 42 S03 = S03  -- Still might be a comment end  : Stay on   state #3
    f  _ S03 = S02  -- Not a comment after all       : Return to state #2

    f  _ S00 = S00  -- Outside of a comment: Stay on state #0

    f  _ S02 = S02  -- Inside a comment    : Stay on state #2

    f  _ _   = S00
