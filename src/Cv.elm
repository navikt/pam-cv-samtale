module Cv exposing
    ( Cv
    , annenErfaring
    , arbeidserfaring
    , decode
    , fagdokumentasjoner
    , førerkort
    , kurs
    , oppdaterFørerkort
    , sammendrag
    , sertifikater
    , spraakferdighet
    , utdanning
    )

import AnnenErfaring.AnnenErfaring as AnnenErfaring exposing (AnnenErfaring)
import Arbeidserfaring.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Fagdokumentasjon.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon)
import Forerkort.Forerkort as Forerkort exposing (Førerkort)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)
import Kurs.Kurs as Kurs exposing (Kurs)
import Sammendrag exposing (Sammendrag)
import Sertifikat.Sertifikat as Sertifikat exposing (Sertifikat)
import Sprak.Sprak as Språk exposing (Språk)
import Utdanning.Utdanning as Utdanning exposing (Utdanning)


type Cv
    = Cv CvInfo


type alias CvInfo =
    { cvid : Int
    , disponererBil : Bool
    , sistEndretDato : String
    , sistEndretAvNav : Bool
    , arbeidserfaring : List Arbeidserfaring
    , utdanningListe : List Utdanning
    , sertifikater : List Sertifikat
    , førerkort : List Førerkort
    , annenErfaring : List AnnenErfaring
    , kurs : List Kurs
    , spraakferdighet : List Språk
    , fagdokumentasjoner : List Fagdokumentasjon
    , sammendrag : Maybe Sammendrag
    }



------- GETTERS


disponererBil : Cv -> Bool
disponererBil (Cv info) =
    info.disponererBil


sistEndretDato : Cv -> String
sistEndretDato (Cv info) =
    info.sistEndretDato


sistEndretAvNav : Cv -> Bool
sistEndretAvNav (Cv info) =
    info.sistEndretAvNav


arbeidserfaring : Cv -> List Arbeidserfaring
arbeidserfaring (Cv info) =
    info.arbeidserfaring


utdanning : Cv -> List Utdanning
utdanning (Cv info) =
    info.utdanningListe


sertifikater : Cv -> List Sertifikat
sertifikater (Cv info) =
    info.sertifikater


førerkort : Cv -> List Førerkort
førerkort (Cv info) =
    info.førerkort


annenErfaring : Cv -> List AnnenErfaring
annenErfaring (Cv info) =
    info.annenErfaring


kurs : Cv -> List Kurs
kurs (Cv info) =
    info.kurs


spraakferdighet : Cv -> List Språk
spraakferdighet (Cv info) =
    info.spraakferdighet


fagdokumentasjoner : Cv -> List Fagdokumentasjon
fagdokumentasjoner (Cv info) =
    info.fagdokumentasjoner


sammendrag : Cv -> Maybe Sammendrag
sammendrag (Cv info) =
    info.sammendrag



---SETTERS---


oppdaterFørerkort : List Førerkort -> Cv -> Cv
oppdaterFørerkort førerkort_ (Cv cvInfo) =
    Cv { cvInfo | førerkort = førerkort_ }



------DECODE ---


decode : Decoder Cv
decode =
    decodeBackendData
        |> map Cv


decodeBackendData : Decoder CvInfo
decodeBackendData =
    succeed CvInfo
        |> required "cvid" int
        |> required "disponererBil" bool
        |> required "sistEndretDato" string
        |> required "sistEndretAvNav" bool
        |> required "arbeidsErfaring" (list Arbeidserfaring.decode)
        |> required "utdanninger" (list Utdanning.decode)
        |> required "sertifikater" (list Sertifikat.decode)
        |> required "forerkort" (list Forerkort.decode)
        |> required "annenErfaring" (list AnnenErfaring.decode)
        |> required "kurs" (list Kurs.decode)
        |> required "spraakferdighet" (list Språk.decode)
        |> required "fagdokumentasjoner" (list Fagdokumentasjon.decode)
        |> required "sammendrag" (nullable Sammendrag.decode)
