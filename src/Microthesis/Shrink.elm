module Microthesis.Shrink exposing
    ( ShrinkCommand
    , cmdLabel
    )


type ShrinkCommand
    = NoOp


cmdLabel : ShrinkCommand -> String
cmdLabel cmd =
    case cmd of
        NoOp ->
            "NoOp"
