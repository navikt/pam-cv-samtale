module Forerkort.Forerkort exposing (Førerkort, Klasse(..), decode, id, klasse)

import Date exposing (Date)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Førerkort
    = Førerkort ForerkortInfo


type alias ForerkortInfo =
    { id : String
    , klasse : Klasse
    , fraDato : Maybe Date
    , utløperDato : Maybe Date
    }


type Klasse
    = Personbil
    | PersonbilMedTilhenger
    | LettLastebil
    | LettLastebilMedTilhenger
    | Lastebil
    | LastebilMedTilhenger
    | Minibuss
    | MinibussMedTilhenger
    | Buss
    | BussMedTilhenger
    | Moped
    | LettMotorsykkel
    | MellomtungMotorsykkel
    | TungMotorsykkel
    | Traktor
    | Snøscooter


id : Førerkort -> String
id (Førerkort info) =
    info.id


klasse : Førerkort -> Klasse
klasse (Førerkort info) =
    info.klasse



---- Decoder ----


decode : Decoder Førerkort
decode =
    decodeBackendData
        |> Json.Decode.andThen tilForerkortInfo


decodeKlasse : String -> Decoder Klasse
decodeKlasse klasse_ =
    if klasse_ == "B" then
        succeed Personbil

    else if klasse_ == "C" then
        succeed Lastebil

    else if klasse_ == "C1" then
        succeed LettLastebil

    else if klasse_ == "C1E" then
        succeed LettLastebilMedTilhenger

    else if klasse_ == "CE" then
        succeed LastebilMedTilhenger

    else if klasse_ == "D1" then
        succeed Minibuss

    else if klasse_ == "D1E" then
        succeed MinibussMedTilhenger

    else if klasse_ == "D" then
        succeed Buss

    else if klasse_ == "DE" then
        succeed BussMedTilhenger

    else if klasse_ == "AM" then
        succeed Moped

    else if klasse_ == "A1" then
        succeed LettMotorsykkel

    else if klasse_ == "A2" then
        succeed MellomtungMotorsykkel

    else if klasse_ == "A" then
        succeed TungMotorsykkel

    else if klasse_ == "BE" then
        succeed PersonbilMedTilhenger

    else if klasse_ == "T" then
        succeed Traktor

    else if klasse_ == "S" then
        succeed Snøscooter

    else
        fail ("Decoding av klasse feilet. Klarer ikke decode verdi: " ++ klasse_)


decodeDateString : Maybe String -> Maybe Date
decodeDateString dato =
    case dato of
        Just dato_ ->
            case Date.fromIsoString dato_ of
                Ok date__ ->
                    Just date__

                Err err ->
                    Nothing

        Nothing ->
            Nothing


tilForerkortInfo : BackendData -> Decoder Førerkort
tilForerkortInfo backendData =
    backendData.klasse
        |> decodeKlasse
        |> Json.Decode.map (lagForerkort backendData)


lagForerkort : BackendData -> Klasse -> Førerkort
lagForerkort backendData klasse_ =
    Førerkort
        { id = backendData.id
        , klasse = klasse_
        , fraDato = decodeDateString backendData.fraDato
        , utløperDato = decodeDateString backendData.utloperDato
        }


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "id" string
        |> required "klasse" string
        |> required "fraDato" (nullable string)
        |> required "utloperDato" (nullable string)


type alias BackendData =
    { id : String
    , klasse : String
    , fraDato : Maybe String
    , utloperDato : Maybe String
    }
