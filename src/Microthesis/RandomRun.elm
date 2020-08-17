module Microthesis.RandomRun exposing
    ( Chunk
    , RandomRun
    , append
    , compare
    , deleteChunk
    , empty
    , get
    , isEmpty
    , isInBounds
    , length
    , replace
    , replaceChunkWithZero
    , set
    , sortChunk
    , swapIfOutOfOrder
    , toList
    , update
    )

import Array exposing (Array)
import Array.Extra


type alias RandomRun =
    Array Int


type alias Chunk =
    { size : Int
    , startIndex : Int
    }


empty : RandomRun
empty =
    Array.empty


isEmpty : RandomRun -> Bool
isEmpty run =
    Array.isEmpty run


append : Int -> RandomRun -> RandomRun
append n run =
    Array.push n run


isInBounds : Chunk -> RandomRun -> Bool
isInBounds { startIndex, size } randomRun =
    startIndex + size <= length randomRun


length : RandomRun -> Int
length run =
    Array.length run


deleteChunk : Chunk -> RandomRun -> RandomRun
deleteChunk { size, startIndex } run =
    Array.append
        -- before
        (Array.slice 0 startIndex run)
        -- after
        (Array.slice (startIndex + size) (Array.length run) run)


replaceChunkWithZero : Chunk -> RandomRun -> RandomRun
replaceChunkWithZero { size, startIndex } run =
    Array.append
        (Array.append
            -- before
            (Array.slice 0 startIndex run)
            -- zeros
            (Array.repeat size 0)
        )
        -- after
        (Array.slice (startIndex + size) (Array.length run) run)


sortChunk : Chunk -> RandomRun -> RandomRun
sortChunk { size, startIndex } run =
    let
        chunk : Array Int
        chunk =
            Array.slice startIndex (startIndex + size) run

        sortedIndexedChunk : List ( Int, Int )
        sortedIndexedChunk =
            chunk
                |> Array.toList
                |> List.sort
                |> List.indexedMap (\i value -> ( startIndex + i, value ))
    in
    replace sortedIndexedChunk run


replace : List ( Int, Int ) -> RandomRun -> RandomRun
replace replacements run =
    List.foldl
        (\( i, value ) acc -> Array.set i value acc)
        run
        replacements


swapIfOutOfOrder :
    { leftIndex : Int, rightIndex : Int }
    -> RandomRun
    ->
        Maybe
            { newRun : RandomRun
            , newLeftValue : Int
            , newRightValue : Int
            }
swapIfOutOfOrder { leftIndex, rightIndex } run =
    Maybe.map2
        (\left right ->
            if left > right then
                { newRun =
                    replace
                        [ ( leftIndex, right )
                        , ( rightIndex, left )
                        ]
                        run
                , newLeftValue = right
                , newRightValue = left
                }

            else
                { newRun = run
                , newLeftValue = left
                , newRightValue = right
                }
        )
        (get leftIndex run)
        (get rightIndex run)


get : Int -> RandomRun -> Maybe Int
get index randomRun =
    Array.get index randomRun


set : Int -> Int -> RandomRun -> RandomRun
set index value randomRun =
    Array.set index value randomRun


sortKey : RandomRun -> ( Int, List Int )
sortKey run =
    ( length run
    , toList run
    )


compare : RandomRun -> RandomRun -> Order
compare a b =
    Basics.compare (sortKey a) (sortKey b)


toList : RandomRun -> List Int
toList run =
    Array.toList run


update : Int -> (Int -> Int) -> RandomRun -> RandomRun
update index fn run =
    case get index run of
        Nothing ->
            run

        Just value ->
            let
                newValue =
                    fn value
            in
            if newValue < 0 then
                run

            else
                replace [ ( index, fn value ) ] run
