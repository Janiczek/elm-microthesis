module Microthesis.Generator exposing (GenResult(..), Generator, int, map, run, runWith, weightedBool)

import Microthesis.PRNG as PRNG exposing (PRNG(..))
import Microthesis.RandomRun as RandomRun exposing (RandomRun)
import Random


type Generator a
    = Generator (PRNG -> GenResult a)


run : Generator a -> PRNG -> GenResult a
run (Generator fn) prng =
    fn prng


runWith : RandomRun -> Generator a -> GenResult a
runWith counterexample generator =
    run generator (PRNG.hardcoded counterexample)


type GenResult a
    = Generated
        { value : a
        , prng : PRNG
        }
    | Rejected
        { -- TODO might have to do `reason : Maybe String`
          -- if the "hardcoded ran out" reasons show up because of shrinking
          reason : String
        , prng : PRNG
        }


mapResult : (a -> b) -> GenResult a -> GenResult b
mapResult fn result =
    case result of
        Generated { value, prng } ->
            Generated
                { value = fn value
                , prng = prng
                }

        Rejected rejected ->
            Rejected rejected


{-| Based on the PRNG value:

  - either draws and remembers a random number
  - or picks a number from the hardcoded list.

-}
rollDice : Random.Generator Int -> Generator Int
rollDice diceGenerator =
    Generator <|
        \prng ->
            case prng of
                Random random ->
                    let
                        ( diceRoll, newSeed ) =
                            Random.step diceGenerator random.seed
                    in
                    Generated
                        { value = diceRoll
                        , prng =
                            Random
                                { seed = newSeed
                                , run = RandomRun.append diceRoll random.run
                                }
                        }

                Hardcoded hardcoded ->
                    case RandomRun.nextChoice hardcoded.unusedPart of
                        Nothing ->
                            Rejected
                                { reason = "Hardcoded PRNG run out of numbers"
                                , prng = prng
                                }

                        Just ( hardcodedChoice, restOfChoices ) ->
                            Generated
                                { value = hardcodedChoice
                                , prng =
                                    Hardcoded
                                        { hardcoded | unusedPart = restOfChoices }
                                }


map : (a -> b) -> Generator a -> Generator b
map fn generator =
    Generator <|
        \prng ->
            run generator prng
                |> mapResult fn


int : Int -> Generator Int
int n =
    rollDice (Random.int 0 n)


weightedBool : Float -> Generator Bool
weightedBool probability =
    rollDice
        (Random.float 0 1
            |> Random.map
                (\float ->
                    if float <= probability then
                        1

                    else
                        0
                )
        )
        |> map
            (\n ->
                if n == 0 then
                    False

                else
                    True
            )
