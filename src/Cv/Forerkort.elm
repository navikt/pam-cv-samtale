module Cv.Forerkort exposing (Forerkort, Klasse(..), decode, id, klasse)

import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Forerkort
    = Forerkort ForerkortInfo


type alias ForerkortInfo =
    { id : String
    , klasse : Klasse
    }


type Klasse
    = Personbil
    | Lastebil
    | LettLastebil
    | LettLastebilMedTilhenger
    | LastebilMedTilhenger
    | Minibuss
    | MinibussMedTilhenger
    | Buss
    | BussMedTilhenger
    | Moped
    | LettMotorsykkel
    | MellomtungMotorsykkel
    | TungMotorsykkel
    | PersonbilMedTilhenger
    | Traktor
    | Snøscooter


id : Forerkort -> String
id (Forerkort info) =
    info.id


klasse : Forerkort -> Klasse
klasse (Forerkort info) =
    info.klasse



---- Decoder ----


decode : Decoder Forerkort
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


tilForerkortInfo : BackendData -> Decoder Forerkort
tilForerkortInfo backendData =
    Json.Decode.map (lagForerkort backendData)
        (decodeKlasse backendData.klasse)


lagForerkort : BackendData -> Klasse -> Forerkort
lagForerkort backendData klasse_ =
    Forerkort
        { id = backendData.id
        , klasse = klasse_
        }


decodeBackendData : Decoder BackendData
decodeBackendData =
    succeed BackendData
        |> required "id" string
        |> required "klasse" string


type alias BackendData =
    { id : String
    , klasse : String
    }
