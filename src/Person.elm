module Person exposing
    ( Person
    , cvSynligForArbeidsgiver
    , decode
    , decodeBackendData
    , harGodtattVilkår
    , underOppfolging
    , usynligGrunnetArenaFlagg
    )

import Json.Decode exposing (Decoder, bool, map, nullable, string, succeed)
import Json.Decode.Pipeline exposing (required)


type Person
    = Person PersonInfo


type alias PersonInfo =
    { underOppfolging : Bool
    , cvSynligForArbeidsgiver : Bool
    , usynligGrunnetArenaFlagg : Bool
    , godtattVilkaar : Bool
    }


underOppfolging : Person -> Bool
underOppfolging (Person info) =
    info.underOppfolging


cvSynligForArbeidsgiver : Person -> Bool
cvSynligForArbeidsgiver (Person info) =
    info.cvSynligForArbeidsgiver


usynligGrunnetArenaFlagg : Person -> Bool
usynligGrunnetArenaFlagg (Person info) =
    info.usynligGrunnetArenaFlagg


harGodtattVilkår : Person -> Bool
harGodtattVilkår (Person info) =
    info.godtattVilkaar



-- DECODER --


decode : Decoder Person
decode =
    decodeBackendData
        |> map Person


decodeBackendData : Decoder PersonInfo
decodeBackendData =
    succeed PersonInfo
        |> required "underOppfolging" bool
        |> required "cvSynligForArbeidsgiver" bool
        |> required "godtattVilkaar" bool
        |> required "usynligGrunnetArenaFlagg" bool
