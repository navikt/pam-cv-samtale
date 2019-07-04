module Cv.Cv exposing (Cv, decode)

import Cv.AnnenErfaring as AnnenErfaring exposing (AnnenErfaring)
import Cv.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Cv.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon)
import Cv.Forerkort as Forerkort exposing (Forerkort)
import Cv.KompetanseKladd as KompetanseKladd exposing (KompetanseKladd)
import Cv.Kurs as Kurs exposing (Kurs)
import Cv.Sammendrag as Sammendrag exposing (Sammendrag)
import Cv.Sertifikat as Sertifikat exposing (Sertifikat)
import Cv.Spraakferdighet as Spraakferdighet exposing (Spraakferdighet)
import Cv.Utdanning as Utdanning exposing (Utdanning)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Cv
    = Cv CvInfo


type alias CvInfo =
    { cvid : Int
    , disponererBil : Bool
    , sistEndretDato : String
    , sistEndretAvNav : Bool
    , arbeidserfaring : List Arbeidserfaring
    , utdanninger : List Utdanning
    , sertifikater : List Sertifikat
    , forerkort : List Forerkort
    , annenErfaring : List AnnenErfaring
    , kurs : List Kurs
    , spraakferdighet : List Spraakferdighet
    , fagdokumentasjoner : List Fagdokumentasjon
    , kompetanseKladdListe : List KompetanseKladd
    , sammendrag : Maybe Sammendrag
    }



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
        |> required "spraakferdighet" (list Spraakferdighet.decode)
        |> required "fagdokumentasjoner" (list Fagdokumentasjon.decode)
        |> required "kompetanseKladdListe" (list KompetanseKladd.decode)
        |> required "sammendrag" Sammendrag.decode
