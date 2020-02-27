module Jobbprofil.Jobbprofil exposing (..)

import Arbeidserfaring.Yrke as Yrke exposing (Yrke)
import Jobbprofil.Kompetanse as Kompetanse exposing (Kompetanse)
import Jobbprofil.Omrade as Omrade exposing (Omrade)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Jobbprofil
    = Jobbprofil JobbprofilInfo


type alias JobbprofilInfo =
    { id : Int
    , stillingliste : List Yrke
    , kompetanseliste : List Kompetanse
    , geografiliste : List Omrade
    , ansettelsesformliste : List String
    , arbeidstidliste : List String
    , arbeidsdagerliste : List String
    , arbeidstidsordningliste : List String
    , omfangsliste : List String
    , oppstart : Maybe String
    }


type alias StillingInfo =
    { id : Maybe String
    , tittel : Maybe String
    , konseptid : Maybe Int
    , styrk08 : Maybe String
    }


type alias StillingKladdInfo =
    StillingInfo



-- TODO - Clean up Jobbprofil
-- Remove kompetanse? (in new file)


type alias KompetanseInfo =
    { id : Maybe String
    , tittel : Maybe String
    , konseptid : Maybe Int
    }


type alias GeografiInfo =
    { id : Maybe String
    , tittel : Maybe String
    , konseptid : Maybe Int
    , kode : Maybe String
    }


id : Jobbprofil -> Int
id (Jobbprofil info) =
    info.id


stillingliste : Jobbprofil -> List Yrke
stillingliste (Jobbprofil info) =
    info.stillingliste


kompetanseliste : Jobbprofil -> List Kompetanse
kompetanseliste (Jobbprofil info) =
    info.kompetanseliste


geografiliste : Jobbprofil -> List Omrade
geografiliste (Jobbprofil info) =
    info.geografiliste


ansettelsesformliste : Jobbprofil -> List String
ansettelsesformliste (Jobbprofil info) =
    info.ansettelsesformliste


arbeidstidliste : Jobbprofil -> List String
arbeidstidliste (Jobbprofil info) =
    info.arbeidstidliste


arbeidsdagerliste : Jobbprofil -> List String
arbeidsdagerliste (Jobbprofil info) =
    info.arbeidsdagerliste


arbeidstidsordningliste : Jobbprofil -> List String
arbeidstidsordningliste (Jobbprofil info) =
    info.arbeidstidsordningliste


omfangsliste : Jobbprofil -> List String
omfangsliste (Jobbprofil info) =
    info.omfangsliste


oppstart : Jobbprofil -> Maybe String
oppstart (Jobbprofil info) =
    info.oppstart



---- Decoder ----


decode : Decoder Jobbprofil
decode =
    decodeBackendData
        |> andThen tilJobbprofil


tilJobbprofil : BackendData -> Decoder Jobbprofil
tilJobbprofil backendData =
    Json.Decode.succeed (lagJobbprofil backendData)


lagJobbprofil : BackendData -> Jobbprofil
lagJobbprofil backendData =
    Jobbprofil
        { id = backendData.id
        , stillingliste = lagYrker backendData.stillingliste
        , kompetanseliste = lagKompetanser backendData.kompetanseliste
        , geografiliste = lagOmrader backendData.geografiliste
        , ansettelsesformliste = backendData.arbeidsdagerliste
        , arbeidstidliste = backendData.arbeidsdagerliste
        , arbeidsdagerliste = backendData.arbeidsdagerliste
        , arbeidstidsordningliste = backendData.arbeidstidsordningliste
        , omfangsliste = backendData.omfangsliste
        , oppstart = backendData.oppstart
        }


lagYrke : StillingInfo -> Maybe Yrke
lagYrke stillingsInfo =
    Maybe.map3 Yrke.fraString
        stillingsInfo.tittel
        stillingsInfo.styrk08
        (konseptIdToString stillingsInfo.konseptid)


lagYrker : List StillingInfo -> List Yrke
lagYrker stillingsInfo =
    List.filterMap lagYrke stillingsInfo


lagKompetanser : List KompetanseInfo -> List Kompetanse
lagKompetanser kompetanseInfo =
    List.filterMap
        (\x ->
            Maybe.map2 Kompetanse.fraEnkeltElementer
                x.tittel
                x.konseptid
        )
        kompetanseInfo


lagOmrader : List GeografiInfo -> List Omrade
lagOmrader geografiInfo =
    List.filterMap
        (\x ->
            Maybe.map3 Omrade.fraEnkeltElementer
                x.tittel
                x.konseptid
                x.kode
        )
        geografiInfo


konseptIdToString : Maybe Int -> Maybe String
konseptIdToString id_ =
    Just (String.fromInt (Maybe.withDefault 1 id_))


stillingInfoDecoder : Decoder StillingInfo
stillingInfoDecoder =
    map4 StillingInfo
        (maybe (field "id" string))
        (maybe (field "tittel" string))
        (maybe (field "konseptid" int))
        (maybe (field "styrk08" string))


kompetanseInfoDecoder : Decoder KompetanseInfo
kompetanseInfoDecoder =
    map3 KompetanseInfo
        (maybe (field "string" string))
        (maybe (field "tittel" string))
        (maybe (field "konseptid" int))


geografiInfoDecoder : Decoder GeografiInfo
geografiInfoDecoder =
    map4 GeografiInfo
        (maybe (field "id" string))
        (maybe (field "tittel" string))
        (maybe (field "konseptid" int))
        (maybe (field "kode" string))


type alias BackendData =
    { id : Int
    , stillingliste : List StillingInfo
    , kompetanseliste : List KompetanseInfo
    , geografiliste : List GeografiInfo
    , ansettelsesformliste : List String
    , arbeidstidliste : List String
    , arbeidsdagerliste : List String
    , arbeidstidsordningliste : List String
    , omfangsliste : List String
    , oppstart : Maybe String
    }


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "id" int
        |> required "stillingliste" (list stillingInfoDecoder)
        |> required "kompetanseliste" (list kompetanseInfoDecoder)
        |> required "geografiliste" (list geografiInfoDecoder)
        |> required "ansettelsesformliste" (list string)
        |> required "arbeidstidliste" (list string)
        |> required "arbeidsdagerliste" (list string)
        |> required "arbeidstidsordningliste" (list string)
        |> required "omfangsliste" (list string)
        |> required "oppstart" (nullable string)
