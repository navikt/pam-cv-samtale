module Cv.Cv exposing (Cv, decode)

import Cv.AnnenErfaring as AnnenErfaring exposing (AnnenErfaring)
import Cv.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Cv.Sertifikat as Sertifikat exposing (Sertifikat)
import Cv.Utdanning as Utdanning exposing (Utdanning)
import Json.Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type Cv
    = Cv CvInfo


type alias CvInfo =
    { disponererBil : Bool
    , sistEndretDato : String
    , arbeidserfaring : List Arbeidserfaring
    , utdanninger : List Utdanning
    , sertifikater : List Sertifikat
    , andreErfaringer : List AnnenErfaring
    }



{--

disponererBil:

CVdto:

sistEndretDato: String
sistEndretAvNav: Bool -------? Er denne nødvending
arbeidsErfaring: list dto
utdanninger:list dto
sertifikater:list dto
forerkort:list dto
annenErfaring:list dto
kurs: list dto
spraakferdighet:list dto
fagdokumentantasjoner: list dto
kompetanseKladdListe list dto -----?
sammendrag:dto


--}
------DECODE ---


decode : Decoder Cv
decode =
    decodeBackendData
        |> map Cv


decodeBackendData : Decoder CvInfo
decodeBackendData =
    succeed CvInfo
        |> required "disponererBil" bool
        |> required "sistEndretDato" string
        |> required "arbeidsErfaring" (list Arbeidserfaring.decode)
        |> required "utdanninger" (list Utdanning.decode)
        |> required "sertifikater" (list Sertifikat.decode)
