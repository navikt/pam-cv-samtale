module Cv.Cv exposing (Cv, decode, fagdokumentasjoner, sammendrag, spraakferdighet, utdanning)

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
    , utdanningListe : List Utdanning
    , sertifikater : List Sertifikat
    , forerkort : List Forerkort
    , annenErfaring : List AnnenErfaring
    , kurs : List Kurs
    , spraakferdighet : List Spraakferdighet
    , fagdokumentasjoner : List Fagdokumentasjon
    , kompetanseKladdListe : List KompetanseKladd
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


forerkort : Cv -> List Forerkort
forerkort (Cv info) =
    info.forerkort


annenErfaring : Cv -> List AnnenErfaring
annenErfaring (Cv info) =
    info.annenErfaring


kurs : Cv -> List Kurs
kurs (Cv info) =
    info.kurs


spraakferdighet : Cv -> List Spraakferdighet
spraakferdighet (Cv info) =
    info.spraakferdighet


fagdokumentasjoner : Cv -> List Fagdokumentasjon
fagdokumentasjoner (Cv info) =
    info.fagdokumentasjoner


kompetanseKladdListe : Cv -> List KompetanseKladd
kompetanseKladdListe (Cv info) =
    info.kompetanseKladdListe


sammendrag : Cv -> Maybe Sammendrag
sammendrag (Cv info) =
    info.sammendrag



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
        |> required "sammendrag" (nullable Sammendrag.decode)
