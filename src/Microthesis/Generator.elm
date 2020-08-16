module Microthesis.Generator exposing
    ( GenResult(..)
    , Generator
    , andMap
    , andThen
    , bool
    , constant
    , filter
    , int
    , lazy
    , map
    , map2
    , map3
    , map4
    , map5
    , map6
    , map7
    , map8
    , maybe
    , oneOf
    , oneOfValues
    , pair
    , reject
    , result
    , run
    , runWith
    , triple
    , unit
    , weightedBool
    )

import List.Extra
import Microthesis.PRNG as PRNG exposing (PRNG(..))
import Microthesis.RandomRun as RandomRun exposing (RandomRun)
import Random



-- TODO ints
-- TODO lists
-- TODO floats
-- TODO chars
-- TODO strings
-- TODO frequency


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


constant : a -> Generator a
constant value =
    Generator <|
        \prng ->
            Generated
                { value = value
                , prng = prng
                }


reject : String -> Generator a
reject reason =
    Generator <|
        \prng ->
            Rejected
                { reason = reason
                , prng = prng
                }


filter : String -> (a -> Bool) -> Generator a -> Generator a
filter label predicate generator =
    generator
        |> andThen
            (\value ->
                if predicate value then
                    constant value

                else
                    reject <| "filter: " ++ label
            )


andThen : (a -> Generator b) -> Generator a -> Generator b
andThen fn (Generator generator) =
    Generator <|
        \prng ->
            case generator prng of
                Generated g ->
                    let
                        (Generator newGenerator) =
                            fn g.value
                    in
                    newGenerator g.prng

                Rejected r ->
                    Rejected r


map : (a -> b) -> Generator a -> Generator b
map fn (Generator generator) =
    Generator <|
        \prng ->
            case generator prng of
                Generated g ->
                    Generated
                        { value = fn g.value
                        , prng = g.prng
                        }

                Rejected r ->
                    Rejected r


andMap : Generator a -> Generator (a -> b) -> Generator b
andMap =
    map2 (|>)


map2 : (a -> b -> c) -> Generator a -> Generator b -> Generator c
map2 fn (Generator generatorA) (Generator generatorB) =
    Generator <|
        \prng ->
            case generatorA prng of
                Generated a ->
                    case generatorB a.prng of
                        Generated b ->
                            Generated
                                { value = fn a.value b.value
                                , prng = b.prng
                                }

                        Rejected r ->
                            Rejected r

                Rejected r ->
                    Rejected r


map3 : (a -> b -> c -> d) -> Generator a -> Generator b -> Generator c -> Generator d
map3 fn a b c =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c


map4 : (a -> b -> c -> d -> e) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e
map4 fn a b c d =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d


map5 : (a -> b -> c -> d -> e -> f) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f
map5 fn a b c d e =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e


map6 : (a -> b -> c -> d -> e -> f -> g) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g
map6 fn a b c d e f =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e
        |> andMap f


map7 : (a -> b -> c -> d -> e -> f -> g -> h) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g -> Generator h
map7 fn a b c d e f g =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e
        |> andMap f
        |> andMap g


map8 : (a -> b -> c -> d -> e -> f -> g -> h -> i) -> Generator a -> Generator b -> Generator c -> Generator d -> Generator e -> Generator f -> Generator g -> Generator h -> Generator i
map8 fn a b c d e f g h =
    constant fn
        |> andMap a
        |> andMap b
        |> andMap c
        |> andMap d
        |> andMap e
        |> andMap f
        |> andMap g
        |> andMap h


maybe : Generator a -> Generator (Maybe a)
maybe item =
    oneOf
        [ constant Nothing
        , map Just item
        ]


result : Generator x -> Generator a -> Generator (Result x a)
result errGenerator okGenerator =
    oneOf
        [ map Err errGenerator
        , map Ok okGenerator
        ]


oneOf : List (Generator a) -> Generator a
oneOf generators =
    case List.length generators of
        0 ->
            reject "oneOf: empty list"

        length ->
            int (length - 1)
                |> andThen
                    (\i ->
                        case List.Extra.getAt i generators of
                            Nothing ->
                                reject "oneOf: bug - didn't find a generator in the list"

                            Just generator ->
                                generator
                    )


oneOfValues : List a -> Generator a
oneOfValues items =
    oneOf (List.map constant items)


bool : Generator Bool
bool =
    oneOfValues [ True, False ]


pair : Generator a -> Generator b -> Generator ( a, b )
pair a b =
    map2 Tuple.pair a b


triple : Generator a -> Generator b -> Generator c -> Generator ( a, b, c )
triple a b c =
    map3 (\ax bx cx -> ( ax, bx, cx )) a b c


lazy : (() -> Generator a) -> Generator a
lazy fn =
    Generator <|
        \prng ->
            let
                (Generator generator) =
                    fn ()
            in
            generator prng


unit : Generator ()
unit =
    constant ()
