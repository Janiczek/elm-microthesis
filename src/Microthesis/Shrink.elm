module Microthesis.Shrink exposing
    ( ShrinkCmd(..)
    , cmdLabel
    , cmdsForRun
    )

import Microthesis.PRNG as PRNG exposing (PRNG)
import Microthesis.RandomRun as RandomRun exposing (Chunk, RandomRun)
import OurExtras.List


type ShrinkCmd
    = DeleteChunkAndMaybeDecrementPrevious Chunk
    | ReplaceChunkWithZero Chunk
    | SortChunk Chunk
    | MinimizeChoiceWithBinarySearch { index : Int }
    | RedistributeChoices { leftIndex : Int, rightIndex : Int }


cmdLabel : ShrinkCmd -> String
cmdLabel cmd =
    case cmd of
        DeleteChunkAndMaybeDecrementPrevious { size, startIndex } ->
            "Delete chunk and maybe decrement previous (chunk size "
                ++ String.fromInt size
                ++ ", start index "
                ++ String.fromInt startIndex
                ++ ")"

        ReplaceChunkWithZero { size, startIndex } ->
            "Replace chunk with zero (chunk size "
                ++ String.fromInt size
                ++ ", start index "
                ++ String.fromInt startIndex
                ++ ")"

        SortChunk { size, startIndex } ->
            "Sort chunk (chunk size "
                ++ String.fromInt size
                ++ ", start index "
                ++ String.fromInt startIndex
                ++ ")"

        MinimizeChoiceWithBinarySearch { index } ->
            "Minimize choice with binary search (index "
                ++ String.fromInt index
                ++ ")"

        RedistributeChoices { leftIndex, rightIndex } ->
            "Redistribute choices (left index "
                ++ String.fromInt leftIndex
                ++ ", right index "
                ++ String.fromInt rightIndex
                ++ ")"


cmdsForRun : RandomRun -> List ShrinkCmd
cmdsForRun run =
    let
        length =
            RandomRun.length run
    in
    OurExtras.List.fastConcat
        [ deletionCmds length
        , zeroCmds length
        , binarySearchCmds length
        , sortCmds length
        , redistributeCmds length
        ]


deletionCmds : Int -> List ShrinkCmd
deletionCmds length =
    chunkCmds
        DeleteChunkAndMaybeDecrementPrevious
        { length = length
        , allowChunksOfSize1 = True
        }


zeroCmds : Int -> List ShrinkCmd
zeroCmds length =
    chunkCmds
        ReplaceChunkWithZero
        { length = length
        , allowChunksOfSize1 = False -- already happens in binary search shrinking
        }


sortCmds : Int -> List ShrinkCmd
sortCmds length =
    chunkCmds
        SortChunk
        { length = length
        , allowChunksOfSize1 = False -- doesn't make sense for sorting
        }


binarySearchCmds : Int -> List ShrinkCmd
binarySearchCmds length =
    List.range 0 (length - 1)
        |> List.reverse
        |> List.map (\index -> MinimizeChoiceWithBinarySearch { index = index })


redistributeCmds : Int -> List ShrinkCmd
redistributeCmds length =
    let
        forOffset : Int -> List ShrinkCmd
        forOffset offset =
            if offset >= length then
                []

            else
                List.range 0 (length - 1 - offset)
                    |> List.reverse
                    |> List.map
                        (\leftIndex ->
                            RedistributeChoices
                                { leftIndex = leftIndex
                                , rightIndex = leftIndex + offset
                                }
                        )
    in
    forOffset 2 ++ forOffset 1


chunkCmds :
    ({ size : Int, startIndex : Int } -> cmd)
    -> { length : Int, allowChunksOfSize1 : Bool }
    -> List cmd
chunkCmds toCmd { length, allowChunksOfSize1 } =
    let
        initChunkSize : Int
        initChunkSize =
            if allowChunksOfSize1 then
                1

            else
                2

        go : Int -> Int -> List cmd -> List cmd
        go chunkSize startIndex acc =
            if startIndex > length - chunkSize then
                if chunkSize == 8 then
                    acc

                else
                    go (chunkSize * 2) 0 acc

            else
                let
                    newCmd =
                        toCmd
                            { size = chunkSize
                            , startIndex = startIndex
                            }
                in
                go chunkSize (startIndex + 1) (newCmd :: acc)
    in
    go initChunkSize 0 []
