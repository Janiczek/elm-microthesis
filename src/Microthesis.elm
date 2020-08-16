module Microthesis exposing
    ( RandomRun
    , Test, test
    , Options, defaultOptions
    , run, runWith, TestResult(..), Bug(..)
    )

{-|

@docs RandomRun

@docs Test, test

@docs Options, defaultOptions

@docs run, runWith, TestResult, Bug

-}

import Dict exposing (Dict)
import Microthesis.Generator as Generator exposing (GenResult(..), Generator)
import Microthesis.PRNG as PRNG
import Microthesis.RandomRun exposing (RandomRun)
import Microthesis.Shrink as Shrink exposing (ShrinkCommand)
import Random


type alias Options =
    { maxExamples : Int
    , showShrinkHistory : Bool
    }


defaultOptions : Options
defaultOptions =
    { maxExamples = 100
    , showShrinkHistory = False
    }


run : Int -> Test a -> TestResult a
run seed test_ =
    runWith defaultOptions seed test_


runWith : Options -> Int -> Test a -> TestResult a
runWith options seed test_ =
    initLoop options seed test_
        |> generateAndTest
        |> shrink
        |> toResult


type Test a
    = Test
        { label : String
        , userTestFn : a -> Bool
        , generator : Generator a
        }


test : String -> Generator a -> (a -> Bool) -> Test a
test label generator userTestFn =
    Test
        { label = label
        , generator = generator
        , userTestFn = userTestFn
        }


type alias RandomRun =
    List Int


type TestResult a
    = Passes
    | FailsWith a
    | FailsWithShrinks
        { finalValue : a
        , finalRun : RandomRun
        , history :
            List
                { value : a
                , run : RandomRun
                , shrinkerUsed : String
                }
        }
    | CannotGenerateValues { mostCommonRejections : List String }
    | MicrothesisBug Bug


type Bug
    = StuckInUndecided


type alias LoopState a =
    -- user input
    { label : String
    , userTestFn : a -> Bool
    , generator : Generator a
    , options : Options
    , maxGenerationAttempts : Int
    , seed : Random.Seed

    -- actual loop state
    , status : Status a
    , generationAttempts : Int
    , valuesGenerated : Int
    , passingTests : Int
    , rejections : Dict String Int
    , shrinkHistory : List ( a, RandomRun, Maybe ShrinkCommand )
    }


type Status a
    = Undecided
    | Passing
    | FailingWith
        { value : a
        , randomRun : RandomRun
        }
    | UnableToGenerate


initLoop : Options -> Int -> Test a -> LoopState a
initLoop options seed (Test test_) =
    { label = test_.label
    , userTestFn = test_.userTestFn
    , generator = test_.generator
    , options = options
    , maxGenerationAttempts = options.maxExamples * 10
    , seed = Random.initialSeed seed
    , status = Undecided
    , generationAttempts = 0
    , valuesGenerated = 0
    , passingTests = 0
    , rejections = Dict.empty
    , shrinkHistory = []
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
                        |> setStatus
                            (FailingWith
                                { value = value
                                , randomRun = counterexampleRun
                                }
                            )

    else if sawPassingTests state then
        setStatus Passing state

    else
        setStatus UnableToGenerate state


shouldTryToGenerate : LoopState a -> Bool
shouldTryToGenerate state =
    (state.valuesGenerated < state.options.maxExamples)
        && (state.generationAttempts < state.maxGenerationAttempts)


generate : LoopState a -> Result (LoopState a) ( RandomRun, a, LoopState a )
generate state =
    case Generator.run state.generator (PRNG.random state.seed) of
        Generated { value, prng } ->
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

        Rejected { reason, prng } ->
            Err
                { state
                    | generationAttempts = state.generationAttempts + 1
                    , seed =
                        PRNG.getSeed prng
                            |> Maybe.withDefault state.seed
                    , rejections =
                        state.rejections
                            |> addRejection reason
                }


addRejection : String -> Dict String Int -> Dict String Int
addRejection reason rejections =
    rejections
        |> Dict.update reason
            (\maybeN ->
                case maybeN of
                    Just n ->
                        Just (n + 1)

                    Nothing ->
                        Just 1
            )


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

        FailingWith { value, randomRun } ->
            if state.options.showShrinkHistory then
                FailsWithShrinks
                    { finalValue = value
                    , finalRun = randomRun
                    , history =
                        state.shrinkHistory
                            |> List.reverse
                            |> List.map
                                (\( value_, run_, maybeCmd ) ->
                                    { value = value_
                                    , run = run_
                                    , shrinkerUsed =
                                        case maybeCmd of
                                            Nothing ->
                                                "Initial"

                                            Just cmd ->
                                                Shrink.cmdLabel cmd
                                    }
                                )
                    }

            else
                FailsWith value

        UnableToGenerate ->
            CannotGenerateValues
                { mostCommonRejections =
                    state.rejections
                        |> Dict.toList
                        |> List.sortBy Tuple.second
                        |> List.take 3
                        |> List.map Tuple.first
                }
