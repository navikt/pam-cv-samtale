module Jobbprofil.Jobbprofil exposing (..)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Jobbprofil
    = Jobbprofil JobbprofilInfo


type alias JobbprofilInfo =
    { id : Int
    , aktiv : Bool
    , stillingliste : List StillingInfo
    , stillingKladdListe : List StillingKladdInfo
    , kompetanseliste : List KompetanseInfo
    , geografiliste : List GeografiInfo
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


aktiv : Jobbprofil -> Bool
aktiv (Jobbprofil info) =
    info.aktiv


stillingliste : Jobbprofil -> List StillingInfo
stillingliste (Jobbprofil info) =
    info.stillingliste


stillingKladdListe : Jobbprofil -> List StillingKladdInfo
stillingKladdListe (Jobbprofil info) =
    info.stillingKladdListe


kompetanseliste : Jobbprofil -> List KompetanseInfo
kompetanseliste (Jobbprofil info) =
    info.kompetanseliste


geografiliste : Jobbprofil -> List GeografiInfo
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
        |> map Jobbprofil


stillingKladdInfoDecoder : Decoder StillingKladdInfo
stillingKladdInfoDecoder =
    stillingInfoDecoder


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
    , aktiv : Bool
    , stillingliste : List StillingInfo
    , stillingKladdListe : List StillingKladdInfo
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
        |> required "aktiv" bool
        |> required "stillingliste" (list stillingInfoDecoder)
        |> required "stillingKladdListe" (list stillingKladdInfoDecoder)
        |> required "kompetanseliste" (list kompetanseInfoDecoder)
        |> required "geografiliste" (list geografiInfoDecoder)
        |> required "ansettelsesformliste" (list string)
        |> required "arbeidstidliste" (list string)
        |> required "arbeidsdagerliste" (list string)
        |> required "arbeidstidsordningliste" (list string)
        |> required "omfangsliste" (list string)
        |> required "oppstart" (nullable string)
