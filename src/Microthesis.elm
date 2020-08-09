module Microthesis exposing (run)

import Microthesis.Generator as Generator exposing (GenResult(..), Generator)
import Microthesis.PRNG as PRNG
import Microthesis.RandomRun exposing (RandomRun)
import Random


run : Test a -> TestResult a
run test =
    test
        |> initLoop
        |> generateAndTest
        |> shrink
        |> toResult


type alias Test a =
    { userTestFn : a -> Bool
    , generator : Generator a
    }


type TestResult a
    = Passes
    | FailsWith a
    | CannotGenerateValues
    | MicrothesisBug Bug


type Bug
    = StuckInUndecided


type alias LoopState a =
    -- user input
    { userTestFn : a -> Bool
    , generator : Generator a
    , seed : Random.Seed

    -- actual loop state
    , status : Status a
    , generationAttempts : Int
    , valuesGenerated : Int
    , passingTests : Int
    }


type Status a
    = Undecided
    | Passing
    | FailingWith ( a, RandomRun )
    | UnableToGenerate


initialSeed : Int
initialSeed =
    0


maxValuesGenerated : Int
maxValuesGenerated =
    100


maxGenerationAttempts : Int
maxGenerationAttempts =
    maxValuesGenerated * 10


initLoop : Test a -> LoopState a
initLoop test =
    { userTestFn = test.userTestFn
    , generator = test.generator
    , seed = Random.initialSeed initialSeed
    , status = Undecided
    , generationAttempts = 0
    , valuesGenerated = 0
    , passingTests = 0
    }


generateAndTest : LoopState a -> LoopState a
generateAndTest state =
    if shouldTryToGenerate state then
        case generate state of
            Err newState ->
                generateAndTest newState

            Ok ( counterexampleRun, value, newState ) ->
                if newState.userTestFn value then
                    generateAndTest
                        { newState | passingTests = newState.passingTests + 1 }

                else
                    newState
                        |> setStatus (FailingWith ( value, counterexampleRun ))

    else if sawPassingTests state then
        setStatus Passing state

    else
        setStatus UnableToGenerate state


shouldTryToGenerate : LoopState a -> Bool
shouldTryToGenerate state =
    (state.valuesGenerated < maxValuesGenerated)
        && (state.generationAttempts < maxGenerationAttempts)


generate : LoopState a -> Result (LoopState a) ( RandomRun, a, LoopState a )
generate state =
    case Generator.run state.generator (PRNG.random state.seed) of
        Generated value prng ->
            Ok
                ( PRNG.getRun prng
                , value
                , { state
                    | generationAttempts = state.generationAttempts + 1
                    , valuesGenerated = state.valuesGenerated + 1
                    , seed =
                        PRNG.getSeed prng
                            |> Maybe.withDefault state.seed
                  }
                )

        Rejected prng ->
            Err
                { state
                    | generationAttempts = state.generationAttempts + 1
                    , seed =
                        PRNG.getSeed prng
                            |> Maybe.withDefault state.seed
                }


setStatus : Status a -> LoopState a -> LoopState a
setStatus status state =
    { state | status = status }


sawPassingTests : LoopState a -> Bool
sawPassingTests state =
    state.passingTests > 0


shrink : LoopState a -> LoopState a
shrink state =
    -- TODO
    state


toResult : LoopState a -> TestResult a
toResult state =
    case state.status of
        Undecided ->
            MicrothesisBug StuckInUndecided

        Passing ->
            Passes

        FailingWith ( value, _ ) ->
            FailsWith value

        UnableToGenerate ->
            CannotGenerateValues
