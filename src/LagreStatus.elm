module LagreStatus exposing (LagreStatus, forsøkPåNytt, fraError, init, lagrerEtterUtlogging, lagrerPåFørsteForsøk, setForsøkPåNytt)

import ErrorHandtering as ErrorHåndtering exposing (OperasjonEtterError(..))
import Http


type LagreStatus
    = LagrerFørsteGang
    | LagrerPåNyttEtterUtlogging { forsøkÅLagrePåNyttEtterDetteForsøket : Bool }
    | LagrerPåNyttEtterError Http.Error



--- INIT ---


init : LagreStatus
init =
    LagrerFørsteGang


fraError : Http.Error -> LagreStatus
fraError error =
    if ErrorHåndtering.operasjonEtterError error == LoggInn then
        LagrerPåNyttEtterUtlogging { forsøkÅLagrePåNyttEtterDetteForsøket = False }

    else
        LagrerPåNyttEtterError error



--- GETTERS ---


lagrerPåFørsteForsøk : LagreStatus -> Bool
lagrerPåFørsteForsøk lagreStatus =
    case lagreStatus of
        LagrerFørsteGang ->
            True

        LagrerPåNyttEtterUtlogging _ ->
            False

        LagrerPåNyttEtterError _ ->
            False


lagrerEtterUtlogging : LagreStatus -> Bool
lagrerEtterUtlogging lagreStatus =
    case lagreStatus of
        LagrerPåNyttEtterUtlogging _ ->
            True

        LagrerFørsteGang ->
            False

        LagrerPåNyttEtterError _ ->
            False


forsøkPåNytt : LagreStatus -> Bool
forsøkPåNytt lagreStatus =
    case lagreStatus of
        LagrerFørsteGang ->
            False

        LagrerPåNyttEtterUtlogging { forsøkÅLagrePåNyttEtterDetteForsøket } ->
            forsøkÅLagrePåNyttEtterDetteForsøket

        LagrerPåNyttEtterError error ->
            False



--- SETTERS ---


setForsøkPåNytt : LagreStatus -> LagreStatus
setForsøkPåNytt lagreStatus =
    case lagreStatus of
        LagrerFørsteGang ->
            LagrerFørsteGang

        LagrerPåNyttEtterUtlogging _ ->
            LagrerPåNyttEtterUtlogging { forsøkÅLagrePåNyttEtterDetteForsøket = True }

        LagrerPåNyttEtterError error ->
            LagrerPåNyttEtterError error
