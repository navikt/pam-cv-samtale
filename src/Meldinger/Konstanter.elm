module Meldinger.Konstanter exposing (meldingHøyde, meldingMarginTop, skriveIndikatorHøyde)


meldingMarginTop : Int
meldingMarginTop =
    8


meldingPadding : Int
meldingPadding =
    16


meldingHøyde : Int -> Int
meldingHøyde height =
    let
        meldingHøydeMaks =
            height + (2 * meldingPadding)
    in
    if meldingHøydeMaks <= 56 then
        56

    else
        meldingHøydeMaks


skriveIndikatorHøyde : Int
skriveIndikatorHøyde =
    56
