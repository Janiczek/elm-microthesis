module Microthesis.RandomRun exposing (RandomRun, append, empty, nextChoice)


type alias RandomRun =
    List Int


empty : RandomRun
empty =
    []


nextChoice : RandomRun -> Maybe ( Int, RandomRun )
nextChoice run =
    case run of
        first :: rest ->
            Just ( first, rest )

        [] ->
            Nothing


{-| Inefficient but simple - lets me not do stuff with Arrays in the screencast
-}
append : Int -> RandomRun -> RandomRun
append n run =
    run ++ [ n ]
