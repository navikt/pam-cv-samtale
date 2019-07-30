module Api exposing
    ( hentCv
    , hentPerson
    , hentPersonalia
    , hentSpråkkoder
    , leggTilSammendrag
    , leggTilSpråk
    , leggTilUtdanning
    , logError
    , oppdaterPersonalia
    , opprettCv
    , opprettPerson
    , opprettPersonalia
    )

import Cv.Cv as Cv exposing (Cv)
import Cv.Sammendrag as Sammendrag exposing (Sammendrag)
import Cv.Spraakferdighet exposing (Spraakferdighet)
import Cv.Utdanning exposing (Utdanning)
import Feilmelding exposing (Feilmelding)
import Http exposing (..)
import Json.Decode
import Json.Encode
import Personalia exposing (Personalia)
import Skjema.Personalia
import Skjema.Sprak
import Skjema.Utdanning
import Sprakkoder exposing (Sprakkoder)


hentPerson : (Result Error () -> msg) -> Cmd msg
hentPerson msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person"
        , expect = expectWhatever msgConstructor
        }


opprettPerson : (Result Error () -> msg) -> Cmd msg
opprettPerson msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person"
        , expect = expectWhatever msgConstructor
        , body = emptyBody
        }


hentPersonalia : (Result Error Personalia -> msg) -> Cmd msg
hentPersonalia msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        }


opprettPersonalia : (Result Error Personalia -> msg) -> Cmd msg
opprettPersonalia msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = emptyBody
        }


oppdaterPersonalia : (Result Error Personalia -> msg) -> Skjema.Personalia.PersonaliaSkjema -> String -> Cmd msg
oppdaterPersonalia msgConstructor skjema id =
    put
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = Skjema.Personalia.encode skjema id |> jsonBody
        }


leggTilSpråk : (Result Error (List Spraakferdighet) -> msg) -> Skjema.Sprak.SpråkSkjema -> Cmd msg
leggTilSpråk msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/sprak"
        , expect = expectJson msgConstructor (Json.Decode.list Cv.Spraakferdighet.decode)
        , body = Skjema.Sprak.encode skjema |> jsonBody
        }


hentSpråkkoder : (Result Error (List Sprakkoder) -> msg) -> Cmd msg
hentSpråkkoder msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/koder/sprak"
        , expect = expectJson msgConstructor (Json.Decode.list Sprakkoder.decode)
        }


leggTilSammendrag : (Result Error Sammendrag -> msg) -> String -> Cmd msg
leggTilSammendrag msgConstructor sammendrag =
    put
        { url = "/cv-samtale/api/rest/cv/sammendrag"
        , expect = expectJson msgConstructor Sammendrag.decode
        , body = Json.Encode.object [ ( "sammendrag", Json.Encode.string sammendrag ) ] |> jsonBody
        }


hentCv : (Result Error Cv -> msg) -> Cmd msg
hentCv msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        }


opprettCv : (Result Error Cv -> msg) -> Cmd msg
opprettCv msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        , body = emptyBody
        }


leggTilUtdanning : (Result Error (List Utdanning) -> msg) -> Skjema.Utdanning.UtdanningSkjema -> Cmd msg
leggTilUtdanning msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/utdanning"
        , expect = expectJson msgConstructor (Json.Decode.list Cv.Utdanning.decode)
        , body = Skjema.Utdanning.encode skjema "id" (Skjema.Utdanning.nuskode skjema) |> jsonBody
        }


logError : (Result Error () -> msg) -> Feilmelding -> Cmd msg
logError msgConstructor feilmelding =
    Http.post
        { url = "/cv-samtale/log"
        , expect = expectWhatever msgConstructor
        , body =
            feilmelding
                |> Feilmelding.encode
                |> jsonBody
        }


put :
    { url : String
    , body : Body
    , expect : Expect msg
    }
    -> Cmd msg
put r =
    request
        { method = "PUT"
        , headers = []
        , url = r.url
        , body = r.body
        , expect = r.expect
        , timeout = Nothing
        , tracker = Nothing
        }
