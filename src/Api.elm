module Api exposing
    ( encodeSammendrag
    , getAAreg
    , getAutorisasjonTypeahead
    , getCv
    , getFagbrevTypeahead
    , getMesterbrevTypeahead
    , getPerson
    , getPersonalia
    , getSertifikatTypeahead
    , getSpråkkoder
    , getYrkeTypeahead
    , hentPoststed
    , logError
    , logErrorWithRequestBody
    , postArbeidserfaring
    , postCv
    , postFagdokumentasjon
    , postPerson
    , postPersonalia
    , postSertifikat
    , postSpråk
    , postSynlighet
    , postUtdanning
    , putArbeidserfaring
    , putPersonalia
    , putSammendrag
    , putSertifikat
    , putUtdanning
    )

import Arbeidserfaring.Skjema
import Cv.Arbeidserfaring as Arbeidserfaring exposing (Arbeidserfaring)
import Cv.Cv as Cv exposing (Cv)
import Cv.Fagdokumentasjon as Fagdokumentasjon exposing (Fagdokumentasjon)
import Cv.Sammendrag as Sammendrag exposing (Sammendrag)
import Cv.Sertifikat as Sertifikat exposing (Sertifikat)
import Cv.Spraakferdighet exposing (Spraakferdighet)
import Cv.Utdanning exposing (Utdanning)
import Fagdokumentasjon.Skjema
import Feilmelding exposing (Feilmelding)
import Http exposing (..)
import Json.Decode exposing (Decoder, bool, field)
import Json.Encode
import Konsept exposing (Konsept)
import Person exposing (Person)
import Personalia exposing (Personalia)
import Personalia.Skjema
import Poststed exposing (Poststed)
import Sertifikat.Skjema
import SertifikatTypeahead exposing (SertifikatTypeahead)
import Sprak.Skjema
import SpråkKode exposing (SpråkKode)
import Utdanning.Skjema
import Yrke as YrkeTypahead exposing (Yrke)


getPerson : (Result Error Person -> msg) -> Cmd msg
getPerson msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person"
        , expect = expectJson msgConstructor Person.decode
        }


postPerson : (Result Error Person -> msg) -> Cmd msg
postPerson msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person"
        , expect = expectJson msgConstructor Person.decode
        , body = emptyBody
        }


postSynlighet : (Result Error Bool -> msg) -> Bool -> Cmd msg
postSynlighet msgConstructor synlighet =
    Http.post
        { url = "/cv-samtale/api/rest/person/synlighet"
        , expect = expectJson msgConstructor (field "synligForArbeidsgiver" bool)
        , body = Json.Encode.object [ ( "synligForArbeidsgiver", Json.Encode.bool synlighet ) ] |> jsonBody
        }


getPersonalia : (Result Error Personalia -> msg) -> Cmd msg
getPersonalia msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        }


postPersonalia : (Result Error Personalia -> msg) -> Cmd msg
postPersonalia msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = emptyBody
        }


putPersonalia : (Result Error Personalia -> msg) -> Personalia.Skjema.ValidertPersonaliaSkjema -> String -> Cmd msg
putPersonalia msgConstructor skjema id =
    put
        { url = "/cv-samtale/api/rest/person/personalia"
        , expect = expectJson msgConstructor Personalia.decode
        , body = Personalia.Skjema.encode skjema id |> jsonBody
        }


postSpråk : (Result Error (List Spraakferdighet) -> msg) -> Sprak.Skjema.SpråkSkjema -> Cmd msg
postSpråk msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/sprak"
        , expect = expectJson msgConstructor (Json.Decode.list Cv.Spraakferdighet.decode)
        , body = Sprak.Skjema.encode skjema |> jsonBody
        }


getSpråkkoder : (Result Error (List SpråkKode) -> msg) -> Cmd msg
getSpråkkoder msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/koder/sprak"
        , expect = expectJson msgConstructor (Json.Decode.list SpråkKode.decode)
        }


hentPoststed : (Result Error Poststed -> msg) -> String -> Cmd msg
hentPoststed msgConstructor postnummer =
    Http.get
        { url = "/cv-samtale/api/rest/koder/poststed?postnummer=" ++ postnummer
        , expect = expectJson msgConstructor Poststed.decode
        }


putSammendrag : (Result Error Sammendrag -> msg) -> String -> Cmd msg
putSammendrag msgConstructor sammendrag =
    put
        { url = "/cv-samtale/api/rest/cv/sammendrag"
        , expect = expectJson msgConstructor Sammendrag.decode
        , body =
            sammendrag
                |> encodeSammendrag
                |> jsonBody
        }


encodeSammendrag : String -> Json.Encode.Value
encodeSammendrag sammendrag =
    Json.Encode.object [ ( "sammendrag", Json.Encode.string sammendrag ) ]


getCv : (Result Error Cv -> msg) -> Cmd msg
getCv msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        }


postCv : (Result Error Cv -> msg) -> Cmd msg
postCv msgConstructor =
    Http.post
        { url = "/cv-samtale/api/rest/cv"
        , expect = expectJson msgConstructor Cv.decode
        , body = emptyBody
        }


getAAreg : (Result Error (List Arbeidserfaring) -> msg) -> Cmd msg
getAAreg msgConstructor =
    Http.get
        { url = "/cv-samtale/api/rest/cv/aareg"
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        }


postArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> Arbeidserfaring.Skjema.ValidertArbeidserfaringSkjema -> Cmd msg
postArbeidserfaring msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/v2/arbeidserfaring"
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        , body = Arbeidserfaring.Skjema.encode skjema |> jsonBody
        }


putArbeidserfaring : (Result Error (List Arbeidserfaring) -> msg) -> Arbeidserfaring.Skjema.ValidertArbeidserfaringSkjema -> String -> Cmd msg
putArbeidserfaring msgConstructor skjema id =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/cv-samtale/api/rest/cv/v2/arbeidserfaring/" ++ id
        , expect = expectJson msgConstructor (Json.Decode.list Arbeidserfaring.decode)
        , body = Arbeidserfaring.Skjema.encode skjema |> jsonBody
        , timeout = Nothing
        , tracker = Nothing
        }


postSertifikat : (Result Error (List Sertifikat) -> msg) -> Sertifikat.Skjema.ValidertSertifikatSkjema -> Cmd msg
postSertifikat msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/sertifikat"
        , expect = expectJson msgConstructor (Json.Decode.list Sertifikat.decode)
        , body = Sertifikat.Skjema.encode skjema |> jsonBody
        }


putSertifikat : (Result Error (List Sertifikat) -> msg) -> Sertifikat.Skjema.ValidertSertifikatSkjema -> String -> Cmd msg
putSertifikat msgConstructor skjema id =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/cv-samtale/api/rest/cv/sertifikat/" ++ id
        , expect = expectJson msgConstructor (Json.Decode.list Sertifikat.decode)
        , body = Sertifikat.Skjema.encode skjema |> jsonBody
        , timeout = Nothing
        , tracker = Nothing
        }


getYrkeTypeahead : (Result Error (List Yrke) -> msg) -> String -> Cmd msg
getYrkeTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/yrke?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list YrkeTypahead.decode)
        }


postUtdanning : (Result Error (List Utdanning) -> msg) -> Utdanning.Skjema.ValidertUtdanningSkjema -> Cmd msg
postUtdanning msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/utdanning"
        , expect = expectJson msgConstructor (Json.Decode.list Cv.Utdanning.decode)
        , body = Utdanning.Skjema.encode skjema |> jsonBody
        }


putUtdanning : (Result Error (List Utdanning) -> msg) -> Utdanning.Skjema.ValidertUtdanningSkjema -> String -> Cmd msg
putUtdanning msgConstructor skjema id =
    Http.request
        { method = "PUT"
        , headers = []
        , url = "/cv-samtale/api/rest/cv/utdanning/" ++ id
        , expect = expectJson msgConstructor (Json.Decode.list Cv.Utdanning.decode)
        , body =
            skjema
                |> Utdanning.Skjema.encode
                |> jsonBody
        , timeout = Nothing
        , tracker = Nothing
        }


postFagdokumentasjon : (Result Error (List Fagdokumentasjon) -> msg) -> Fagdokumentasjon.Skjema.ValidertFagdokumentasjonSkjema -> Cmd msg
postFagdokumentasjon msgConstructor skjema =
    Http.post
        { url = "/cv-samtale/api/rest/cv/fagdokumentasjon"
        , expect = expectJson msgConstructor (Json.Decode.list Fagdokumentasjon.decode)
        , body =
            skjema
                |> Fagdokumentasjon.Skjema.encode
                |> jsonBody
        }


getFagbrevTypeahead : (Result Error (List Konsept) -> msg) -> String -> Cmd msg
getFagbrevTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/fagbrev?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list Konsept.decode)
        }


getMesterbrevTypeahead : (Result Error (List Konsept) -> msg) -> String -> Cmd msg
getMesterbrevTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/mesterbrev?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list Konsept.decode)
        }


getAutorisasjonTypeahead : (Result Error (List Konsept) -> msg) -> String -> Cmd msg
getAutorisasjonTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/autorisasjoner?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list Konsept.decode)
        }


getSertifikatTypeahead : (Result Error (List SertifikatTypeahead) -> msg) -> String -> Cmd msg
getSertifikatTypeahead msgConstructor string =
    Http.get
        { url = "/cv-samtale/api/rest/typeahead/autorisasjon?q=" ++ string
        , expect = expectJson msgConstructor (Json.Decode.list SertifikatTypeahead.decode)
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


logErrorWithRequestBody : msg -> String -> Http.Error -> Json.Encode.Value -> Cmd msg
logErrorWithRequestBody msg operation error requestBody =
    Feilmelding.feilmelding operation error
        |> Maybe.map (Feilmelding.withRequestBody requestBody)
        |> Maybe.map (logError (always msg))
        |> Maybe.withDefault Cmd.none


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
