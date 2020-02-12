module Person exposing
    ( BrukerInfo(..)
    , Person
    , Synlighet(..)
    , brukerInfo
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


type Synlighet
    = Synlig
    | IkkeSynlig


type BrukerInfo
    = JobbSkifter Synlighet
    | UnderOppfølging Synlighet


type alias PersonInfo =
    { underOppfolging : Bool
    , cvSynligForArbeidsgiver : Bool
    , godtattVilkaar : Bool
    , usynligGrunnetArenaFlagg : Bool
    }


brukerInfo : Person -> BrukerInfo
brukerInfo (Person info) =
    if info.underOppfolging then
        if info.usynligGrunnetArenaFlagg then
            UnderOppfølging IkkeSynlig

        else
            UnderOppfølging Synlig

    else if info.cvSynligForArbeidsgiver then
        JobbSkifter Synlig

    else
        JobbSkifter IkkeSynlig


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
