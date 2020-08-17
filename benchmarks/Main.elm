module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram)
import Microthesis
import Microthesis.Generator as Gen
import Random


main =
    Benchmark.Runner.program suite


suite : Benchmark
suite =
    Benchmark.describe "Microthesis"
        [ {- benchmark_ "bool"
                 Gen.bool
             , benchmark_ "int 0 10"
                 (Gen.int 10)
             , benchmark_ "weightedBool 0.75"
                 (Gen.weightedBool 0.75)
             , benchmark_ "unit"
                 Gen.unit
             , benchmark_ "triple ints"
                 (Gen.triple
                     (Gen.int 10)
                     (Gen.int 10)
                     (Gen.int 10)
                 )
             ,
          -}
          benchmark_ "list of ints"
            (Gen.list (Gen.int 10))
        ]


benchmark_ :
    String
    -> Gen.Generator a
    -> Benchmark
benchmark_ label generator =
    let
        genAndShrink seed () =
            Microthesis.runWith
                { maxExamples = 100
                , showShrinkHistory = False
                }
                seed
                (Microthesis.test
                    label
                    generator
                    (always False)
                )

        gen seed () =
            Gen.generateWithSeed seed generator
    in
    describe label
        [ benchmark "(0) gen" (gen (Random.initialSeed 0))
        , benchmark "(1) gen" (gen (Random.initialSeed 1))
        , benchmark "(2) gen" (gen (Random.initialSeed 2))
        , benchmark "(3) gen" (gen (Random.initialSeed 3))
        , benchmark "(4) gen" (gen (Random.initialSeed 4))
        , benchmark "(0) gen + shrink" (genAndShrink 0)
        , benchmark "(1) gen + shrink" (genAndShrink 1)
        , benchmark "(2) gen + shrink" (genAndShrink 2)
        , benchmark "(3) gen + shrink" (genAndShrink 3)
        , benchmark "(4) gen + shrink" (genAndShrink 4)
        ]
