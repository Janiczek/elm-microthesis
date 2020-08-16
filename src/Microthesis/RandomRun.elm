module Microthesis.RandomRun exposing
    ( Chunk
    , RandomRun
    , append
    , deleteChunk
    , empty
    , get
    , isEmpty
    , isInBounds
    , length
    , nextChoice
    , replace
    , replaceChunkWithZero
    , set
    , sortChunk
    , swapIfOutOfOrder
    )

import List.Extra
import OurExtras.List


type alias RandomRun =
    List Int


type alias Chunk =
    { size : Int
    , startIndex : Int
    }


empty : RandomRun
empty =
    []


isEmpty : RandomRun -> Bool
isEmpty run =
    List.isEmpty run


nextChoice : RandomRun -> Maybe ( Int, RandomRun )
nextChoice run =
    case run of
        first :: rest ->
            Just ( first, rest )

        [] ->
            Nothing


append : Int -> RandomRun -> RandomRun
append n run =
    {- NOTE: inefficient, reversed list would be more performant but might make
       the logic impenetrable
    -}
    run ++ [ n ]


isInBounds : Chunk -> RandomRun -> Bool
isInBounds { startIndex, size } randomRun =
    startIndex + size <= length randomRun


length : RandomRun -> Int
length run =
    List.length run


getChunk : Chunk -> RandomRun -> List Int
getChunk chunk run =
    run
        |> List.drop chunk.startIndex
        |> List.take chunk.size


deleteChunk : Chunk -> RandomRun -> RandomRun
deleteChunk chunk run =
    List.take chunk.startIndex run
        ++ List.drop (chunk.startIndex + chunk.size) run


replaceChunkWithZero : Chunk -> RandomRun -> RandomRun
replaceChunkWithZero chunk run =
    -- TODO maybe `replace [...] run` would be faster?
    OurExtras.List.fastConcat
        [ List.take chunk.startIndex run
        , List.repeat chunk.size 0
        , List.drop (chunk.startIndex + chunk.size) run
        ]


sortChunk : Chunk -> RandomRun -> RandomRun
sortChunk chunk run =
    let
        sortedIndexed : List ( Int, Int )
        sortedIndexed =
            run
                |> getChunk chunk
                |> List.sort
                |> List.indexedMap
                    (\i value -> ( chunk.startIndex + i, value ))
    in
    replace sortedIndexed run


replace : List ( Int, Int ) -> RandomRun -> RandomRun
replace values run =
    List.foldl
        (\( index, newValue ) accRun ->
            List.Extra.setAt index newValue accRun
        )
        run
        values


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
        (List.Extra.getAt leftIndex run)
        (List.Extra.getAt rightIndex run)


get : Int -> RandomRun -> Maybe Int
get index randomRun =
    List.Extra.getAt index randomRun


set : Int -> Int -> RandomRun -> RandomRun
set index value randomRun =
    List.Extra.setAt index value randomRun
