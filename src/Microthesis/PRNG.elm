module Microthesis.PRNG exposing (PRNG(..), getRun, getSeed, hardcoded, random)

import Microthesis.RandomRun as RandomRun exposing (RandomRun)
import Random


type PRNG
    = Random
        { seed : Random.Seed
        , run : RandomRun
        }
    | Hardcoded
        { wholeRun : RandomRun
        , unusedPart : RandomRun
        }


random : Random.Seed -> PRNG
random seed =
    Random
        { seed = seed
        , run = RandomRun.empty
        }


hardcoded : RandomRun -> PRNG
hardcoded run =
    Hardcoded
        { wholeRun = run
        , unusedPart = run
        }


getRun : PRNG -> RandomRun
getRun prng =
    case prng of
        Random { run } ->
            run

        Hardcoded { wholeRun } ->
            wholeRun


getSeed : PRNG -> Maybe Random.Seed
getSeed prng =
    case prng of
        Random { seed } ->
            Just seed

        Hardcoded _ ->
            Nothing
