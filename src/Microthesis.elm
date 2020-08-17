module Microthesis exposing
    ( PrngHistory
    , Test, test
    , Options, defaultOptions
    , run, runWith, TestResult(..), Bug(..)
    )

{-|

@docs PrngHistory

@docs Test, test

@docs Options, defaultOptions

@docs run, runWith, TestResult, Bug

-}

import Dict exposing (Dict)
import Microthesis.Generator as Generator exposing (GenResult(..), Generator)
import Microthesis.PRNG as PRNG
import Microthesis.RandomRun as RandomRun exposing (Chunk, RandomRun)
import Microthesis.Shrink as Shrink exposing (ShrinkCmd(..))
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


type alias PrngHistory =
    List Int


type TestResult a
    = Passes
    | FailsWith a
    | FailsWithShrinks
        { finalValue : a
        , finalRun : PrngHistory
        , history :
            List
                { value : a
                , run : PrngHistory
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
    , shrinkHistory : List ( a, RandomRun, Maybe ShrinkCmd )
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
    case state.status of
        FailingWith { randomRun, value } ->
            if RandomRun.isEmpty randomRun then
                state

            else
                state
                    |> addShrinkToHistory value randomRun Nothing
                    |> shrinkWhileProgress randomRun

        Undecided ->
            state

        Passing ->
            state

        UnableToGenerate ->
            state


addShrinkToHistory : a -> RandomRun -> Maybe ShrinkCmd -> LoopState a -> LoopState a
addShrinkToHistory value randomRun maybeCmd state =
    { state
        | shrinkHistory =
            ( value, randomRun, maybeCmd ) :: state.shrinkHistory
    }


shrinkWhileProgress : RandomRun -> LoopState a -> LoopState a
shrinkWhileProgress randomRun state =
    let
        ( nextRun, nextState ) =
            shrinkOnce randomRun state
    in
    if nextRun == randomRun then
        nextState

    else
        shrinkWhileProgress nextRun nextState


shrinkOnce : RandomRun -> LoopState a -> ( RandomRun, LoopState a )
shrinkOnce randomRun state =
    runCmds
        (Shrink.cmdsForRun randomRun)
        randomRun
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
                    , finalRun = RandomRun.toList randomRun
                    , history =
                        state.shrinkHistory
                            |> List.reverse
                            |> List.map
                                (\( value_, run_, maybeCmd ) ->
                                    { value = value_
                                    , run = RandomRun.toList run_
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


tryShrink :
    { old : RandomRun
    , new : RandomRun
    , cmd : ShrinkCmd
    }
    -> LoopState a
    ->
        { foundImprovement : Bool
        , finalRun : RandomRun
        , finalState : LoopState a
        }
tryShrink { old, new, cmd } state =
    let
        nope =
            { foundImprovement = False
            , finalRun = old
            , finalState = state
            }
    in
    if old == new then
        nope

    else
        case Generator.run state.generator (PRNG.hardcoded new) of
            Generated { value } ->
                if state.userTestFn value then
                    nope

                else if RandomRun.compare old new == GT then
                    { foundImprovement = True
                    , finalRun = new
                    , finalState =
                        state
                            |> addShrinkToHistory value new (Just cmd)
                            |> setStatus
                                (FailingWith
                                    { value = value
                                    , randomRun = new
                                    }
                                )
                    }

                else
                    nope

            Rejected _ ->
                nope


runCmds : List ShrinkCmd -> RandomRun -> LoopState a -> ( RandomRun, LoopState a )
runCmds cmds randomRun state =
    List.foldl
        runCmd
        ( randomRun, state )
        cmds


runCmd : ShrinkCmd -> ( RandomRun, LoopState a ) -> ( RandomRun, LoopState a )
runCmd cmd ( randomRun, state ) =
    case cmd of
        DeleteChunkAndMaybeDecrementPrevious chunk ->
            deleteChunkAndMaybeDecrementPrevious cmd chunk randomRun state

        ReplaceChunkWithZero chunk ->
            replaceChunkWithZero cmd chunk randomRun state

        SortChunk chunk ->
            sortChunk cmd chunk randomRun state

        MinimizeChoiceWithBinarySearch options ->
            minimizeWithBinarySearch cmd options randomRun state

        RedistributeChoices options ->
            redistribute cmd options randomRun state


deleteChunkAndMaybeDecrementPrevious : ShrinkCmd -> Chunk -> RandomRun -> LoopState a -> ( RandomRun, LoopState a )
deleteChunkAndMaybeDecrementPrevious cmd chunk randomRun state =
    if RandomRun.isInBounds chunk randomRun then
        let
            runWithDelete : RandomRun
            runWithDelete =
                RandomRun.deleteChunk chunk randomRun

            afterDelete =
                tryShrink
                    { old = randomRun
                    , new = runWithDelete
                    , cmd = cmd
                    }
                    state
        in
        if afterDelete.foundImprovement then
            {- Try reducing the number before this removed chunk, it's frequently
               the length parameter.
            -}
            let
                runWithDecrement : RandomRun
                runWithDecrement =
                    runWithDelete
                        |> RandomRun.update (chunk.startIndex - 1) (\x -> x - 1)

                afterDecrement =
                    tryShrink
                        { old = afterDelete.finalRun
                        , new = runWithDecrement
                        , cmd = cmd
                        }
                        afterDelete.finalState
            in
            ( afterDecrement.finalRun, afterDecrement.finalState )

        else
            ( afterDelete.finalRun, afterDelete.finalState )

    else
        ( randomRun, state )


replaceChunkWithZero : ShrinkCmd -> Chunk -> RandomRun -> LoopState a -> ( RandomRun, LoopState a )
replaceChunkWithZero cmd chunk randomRun state =
    if RandomRun.isInBounds chunk randomRun then
        let
            shrunkRun : RandomRun
            shrunkRun =
                RandomRun.replaceChunkWithZero chunk randomRun

            { finalRun, finalState } =
                tryShrink
                    { old = randomRun
                    , new = shrunkRun
                    , cmd = cmd
                    }
                    state
        in
        ( finalRun, finalState )

    else
        ( randomRun, state )


sortChunk : ShrinkCmd -> Chunk -> RandomRun -> LoopState a -> ( RandomRun, LoopState a )
sortChunk cmd chunk randomRun state =
    if RandomRun.isInBounds chunk randomRun then
        let
            shrunkRun : RandomRun
            shrunkRun =
                RandomRun.sortChunk chunk randomRun

            { finalRun, finalState } =
                tryShrink
                    { old = randomRun
                    , new = shrunkRun
                    , cmd = cmd
                    }
                    state
        in
        ( finalRun, finalState )

    else
        ( randomRun, state )


minimizeWithBinarySearch : ShrinkCmd -> { index : Int } -> RandomRun -> LoopState a -> ( RandomRun, LoopState a )
minimizeWithBinarySearch cmd { index } randomRun state =
    if
        RandomRun.isInBounds
            { startIndex = index
            , size = 1
            }
            randomRun
    then
        case RandomRun.get index randomRun of
            Nothing ->
                ( randomRun, state )

            Just value ->
                binarySearchShrink
                    { low = 0
                    , high = value
                    , run = randomRun
                    , state = state
                    , cmd = cmd
                    , updateRun =
                        \value_ accRun ->
                            RandomRun.set index value_ accRun
                    }

    else
        ( randomRun, state )


redistribute : ShrinkCmd -> { leftIndex : Int, rightIndex : Int } -> RandomRun -> LoopState a -> ( RandomRun, LoopState a )
redistribute cmd options randomRun state =
    if
        RandomRun.isInBounds
            { startIndex = options.leftIndex
            , size = options.rightIndex - options.leftIndex + 1
            }
            randomRun
    then
        {- First we try swapping them if left > right.

           Then we try to (binary-search) minimize the left while keeping the
           sum constant (so what we subtract from left we add to right).
        -}
        case RandomRun.swapIfOutOfOrder options randomRun of
            Nothing ->
                ( randomRun, state )

            Just { newRun, newLeftValue, newRightValue } ->
                let
                    { finalRun, finalState } =
                        tryShrink
                            { old = randomRun
                            , new = newRun
                            , cmd = cmd
                            }
                            state
                in
                binarySearchShrink
                    { low = 0
                    , high = newLeftValue
                    , run = finalRun
                    , state = finalState
                    , cmd = cmd
                    , updateRun =
                        \value accRun ->
                            RandomRun.replace
                                [ ( options.leftIndex, value )
                                , ( options.rightIndex, newRightValue + newLeftValue - value )
                                ]
                                accRun
                    }

    else
        ( randomRun, state )


type alias BinarySearchOptions a =
    { low : Int
    , high : Int
    , run : RandomRun
    , state : LoopState a
    , cmd : ShrinkCmd
    , updateRun : Int -> RandomRun -> RandomRun
    }


binarySearchShrink : BinarySearchOptions a -> ( RandomRun, LoopState a )
binarySearchShrink ({ updateRun, low, state, cmd } as options) =
    let
        runWithLow =
            updateRun low options.run

        { foundImprovement, finalRun, finalState } =
            tryShrink
                { old = options.run
                , new = runWithLow
                , cmd = cmd
                }
                state
    in
    if foundImprovement then
        ( finalRun, finalState )

    else
        binarySearchLoop options


binarySearchLoop : BinarySearchOptions a -> ( RandomRun, LoopState a )
binarySearchLoop ({ low, high, updateRun, state, cmd } as options) =
    if low + 1 < high then
        let
            mid =
                (low + high) // 2

            newRun =
                updateRun mid options.run

            { foundImprovement, finalRun, finalState } =
                tryShrink
                    { old = options.run
                    , new = newRun
                    , cmd = cmd
                    }
                    state

            optionsWithNewRange =
                if foundImprovement then
                    { options | high = mid }

                else
                    { options | low = mid }

            newOptions =
                { optionsWithNewRange
                    | run = finalRun
                    , state = finalState
                }
        in
        binarySearchLoop newOptions

    else
        ( options.run, options.state )
