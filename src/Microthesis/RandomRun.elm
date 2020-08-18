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
    , nextChoice
    , replace
    , replaceChunkWithZero
    , set
    , sortChunk
    , swapIfOutOfOrder
    , toList
    , update
    )

import Deque exposing (Deque)


type alias RandomRun =
    Deque Int


type alias Chunk =
    { size : Int
    , startIndex : Int
    }


empty : RandomRun
empty =
    Deque.empty


isEmpty : RandomRun -> Bool
isEmpty run =
    Deque.isEmpty run


nextChoice : RandomRun -> Maybe ( Int, RandomRun )
nextChoice run =
    case Deque.popFront run of
        ( Nothing, _ ) ->
            Nothing

        ( Just first, rest ) ->
            Just ( first, rest )


append : Int -> RandomRun -> RandomRun
append n run =
    Deque.pushBack n run


isInBounds : Chunk -> RandomRun -> Bool
isInBounds { startIndex, size } randomRun =
    startIndex + size <= length randomRun


length : RandomRun -> Int
length run =
    Deque.length run


getChunk : Chunk -> RandomRun -> List Int
getChunk chunk run =
    run
        |> Deque.dropLeft chunk.startIndex
        |> Deque.left chunk.size
        |> Deque.toList


deleteChunk : Chunk -> RandomRun -> RandomRun
deleteChunk chunk run =
    Deque.append
        (Deque.left chunk.startIndex run)
        (Deque.dropLeft (chunk.startIndex + chunk.size) run)


replaceChunkWithZero : Chunk -> RandomRun -> RandomRun
replaceChunkWithZero chunk run =
    -- TODO maybe `replace [...] run` would be faster?
    Deque.append
        (Deque.left chunk.startIndex run)
        (Deque.append
            (Deque.repeat chunk.size 0)
            (Deque.dropLeft (chunk.startIndex + chunk.size) run)
        )


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
            set index newValue accRun
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
        (get leftIndex run)
        (get rightIndex run)


get : Int -> RandomRun -> Maybe Int
get index randomRun =
    randomRun
        |> Deque.dropLeft index
        |> Deque.first


set : Int -> Int -> RandomRun -> RandomRun
set index value randomRun =
    Deque.append
        (Deque.left index randomRun
            |> Deque.pushBack value
        )
        (Deque.dropLeft (index + 1) randomRun)


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
    Deque.toList run


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
