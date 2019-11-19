module Konstanter exposing (meldingHøyde, meldingMarginTop, skriveIndikatorHøyde)


meldingMarginTop : Float
meldingMarginTop =
    8


meldingPadding : Int
meldingPadding =
    16


meldingHøyde : Int -> Float
meldingHøyde height =
    let
        meldingHøydeMaks =
            (height + (2 * meldingPadding))
                |> toFloat
    in
    if meldingHøydeMaks <= 56 then
        56

    else
        meldingHøydeMaks


skriveIndikatorHøyde : Float
skriveIndikatorHøyde =
    56
