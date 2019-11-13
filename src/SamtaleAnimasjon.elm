module SamtaleAnimasjon exposing (Msg, scrollTilBunn, startAnimasjon, subscriptions, update)

import Browser.Dom as Dom exposing (Element, Viewport)
import Browser.Events
import DebugStatus exposing (DebugStatus)
import Ease
import MeldingsLogg exposing (FerdigAnimertStatus(..), MeldingsLogg, ScrollAnimasjonStatus(..))
import Process
import Task exposing (Task)
import Time


scrollTilBunn : (Result Dom.Error () -> msg) -> Cmd msg
scrollTilBunn msgKonstruktør =
    Dom.getViewportOf "samtale"
        |> Task.andThen (\viewportInfo -> Dom.setViewportOf "samtale" 0 (viewportInfo.scene.height - viewportInfo.viewport.height))
        |> Task.attempt msgKonstruktør


type Msg
    = StartÅSkrive (Result Dom.Error ( Viewport, Time.Posix ))
    | StartMeldingsanimasjon
    | RegistrerMeldingsdimensjoner (Result Dom.Error ( Dom.Element, Viewport, Time.Posix ))
    | FullførMeldingsanimasjon
    | VisBrukerInput
    | StartÅScrolleInnInput (Result Dom.Error ( Viewport, Time.Posix ))
    | AnimationFrame Time.Posix
    | ScrolletViewport (Result Dom.Error ())


startAnimasjon : DebugStatus -> Cmd Msg
startAnimasjon debugStatus =
    200
        |> DebugStatus.meldingsTimeout debugStatus
        |> Process.sleep
        |> Task.andThen (\_ -> getTimeAndViewport)
        |> Task.attempt StartÅSkrive


update : DebugStatus -> Msg -> MeldingsLogg -> ( MeldingsLogg, Cmd Msg )
update debugStatus msg meldingsLogg =
    case msg of
        StartÅSkrive result ->
            case result of
                Ok ( viewport, posix ) ->
                    if DebugStatus.hoppOverMeldingsanimasjon debugStatus then
                        hoppOverMeldingsanimasjon meldingsLogg

                    else
                        ( MeldingsLogg.startÅSkrive posix viewport meldingsLogg
                        , MeldingsLogg.nesteMeldingToString meldingsLogg
                            * 1000.0
                            |> DebugStatus.meldingsTimeout debugStatus
                            |> Process.sleep
                            |> Task.perform (always StartMeldingsanimasjon)
                        )

                Err error ->
                    ( meldingsLogg, Cmd.none )

        StartMeldingsanimasjon ->
            let
                nyMeldingslogg =
                    MeldingsLogg.startAnimasjon meldingsLogg
            in
            ( nyMeldingslogg
            , nyMeldingslogg
                |> MeldingsLogg.sisteMeldingId
                |> getDimensionsTimeAndViewport
                |> Task.attempt RegistrerMeldingsdimensjoner
            )

        RegistrerMeldingsdimensjoner result ->
            case result of
                Ok ( element, viewport, posix ) ->
                    ( meldingsLogg
                        |> MeldingsLogg.registrerDimensjoner
                            { height = ceiling element.element.height
                            , width = ceiling element.element.width
                            , viewport = viewport
                            , posix = posix
                            }
                    , 400
                        |> DebugStatus.meldingsTimeout debugStatus
                        |> Process.sleep
                        |> Task.perform (always FullførMeldingsanimasjon)
                    )

                Err error ->
                    ( meldingsLogg, Cmd.none )

        FullførMeldingsanimasjon ->
            let
                nyMeldingslogg =
                    MeldingsLogg.fullførAnimasjon meldingsLogg
            in
            case MeldingsLogg.ferdigAnimert nyMeldingslogg of
                FerdigAnimert _ ->
                    ( nyMeldingslogg
                    , 250
                        |> DebugStatus.meldingsTimeout debugStatus
                        |> Process.sleep
                        |> Task.attempt (always VisBrukerInput)
                    )

                MeldingerGjenstår ->
                    ( nyMeldingslogg
                    , 1000
                        |> DebugStatus.meldingsTimeout debugStatus
                        |> Process.sleep
                        |> Task.andThen (\_ -> getTimeAndViewport)
                        |> Task.attempt StartÅSkrive
                    )

        VisBrukerInput ->
            ( MeldingsLogg.begynnÅViseBrukerInput meldingsLogg
            , getTimeAndViewport
                |> Task.attempt StartÅScrolleInnInput
            )

        StartÅScrolleInnInput result ->
            case result of
                Ok ( viewport, timestamp ) ->
                    ( MeldingsLogg.startScrollingTilInput timestamp viewport meldingsLogg, Cmd.none )

                Err _ ->
                    ( meldingsLogg, Cmd.none )

        ScrolletViewport _ ->
            ( meldingsLogg, Cmd.none )

        AnimationFrame posix ->
            case MeldingsLogg.scrollAnimasjonStatus meldingsLogg of
                IngenScrollAnimasjon ->
                    ( meldingsLogg, Cmd.none )

                ScrollerInnSkriveIndikator record ->
                    scrollTilSkriveIndikator meldingsLogg record posix

                ScrollerInnMelding record ->
                    scrollTilMelding meldingsLogg record posix

                ScrollerInnInputFelt record ->
                    scrollInnBrukerInput meldingsLogg record posix


hoppOverMeldingsanimasjon : MeldingsLogg -> ( MeldingsLogg, Cmd Msg )
hoppOverMeldingsanimasjon meldingsLogg =
    ( MeldingsLogg.debugFullførAlleMeldinger meldingsLogg, scrollTilBunn ScrolletViewport )


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


getDimensionsTimeAndViewport : String -> Task Dom.Error ( Element, Viewport, Time.Posix )
getDimensionsTimeAndViewport id =
    Task.map3
        triple
        (Dom.getElement id)
        (Dom.getViewportOf "samtale")
        Time.now


getTimeAndViewport : Task Dom.Error ( Viewport, Time.Posix )
getTimeAndViewport =
    Task.map2
        Tuple.pair
        (Dom.getViewportOf "samtale")
        Time.now


scrollTilSkriveIndikator : MeldingsLogg -> { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport } -> Time.Posix -> ( MeldingsLogg, Cmd Msg )
scrollTilSkriveIndikator meldingsLogg { startTidForScrolling, opprinneligViewport } tidNå =
    ( meldingsLogg
    , { animasjonstidMs = 400
      , opprinneligViewport = opprinneligViewport
      , sluttPosisjon = opprinneligViewport.scene.height + 62 - opprinneligViewport.viewport.height
      , tidNå = tidNå
      , startTidForScrolling = startTidForScrolling
      }
        |> scrollPosition
        |> Dom.setViewportOf "samtale" 0
        |> Task.attempt ScrolletViewport
    )


scrollTilMelding : MeldingsLogg -> { height : Int, startTidForScrolling : Time.Posix, opprinneligViewport : Viewport } -> Time.Posix -> ( MeldingsLogg, Cmd Msg )
scrollTilMelding meldingsLogg { height, startTidForScrolling, opprinneligViewport } tidNå =
    let
        heightPlussPadding =
            toFloat height + (2 * 16)

        sluttPosisjon =
            opprinneligViewport.scene.height + heightPlussPadding - 54 - opprinneligViewport.viewport.height
    in
    ( meldingsLogg
    , { animasjonstidMs = 400
      , opprinneligViewport = opprinneligViewport
      , sluttPosisjon = sluttPosisjon
      , tidNå = tidNå
      , startTidForScrolling = startTidForScrolling
      }
        |> scrollPosition
        |> Dom.setViewportOf "samtale" 0
        |> Task.attempt ScrolletViewport
    )


scrollPosition :
    { animasjonstidMs : Int
    , opprinneligViewport : Viewport
    , sluttPosisjon : Float
    , tidNå : Time.Posix
    , startTidForScrolling : Time.Posix
    }
    -> Float
scrollPosition { animasjonstidMs, opprinneligViewport, sluttPosisjon, tidNå, startTidForScrolling } =
    let
        startPosisjon =
            opprinneligViewport.viewport.y

        totalAvstand =
            sluttPosisjon - startPosisjon

        tidGått =
            Time.posixToMillis tidNå - Time.posixToMillis startTidForScrolling

        normalisertTid =
            clamp 0 animasjonstidMs tidGått
                |> toFloat

        andelUtførtTid =
            normalisertTid / toFloat animasjonstidMs

        andelUtførtAvstand =
            Ease.inOutCubic andelUtførtTid

        yNå =
            (totalAvstand * andelUtførtAvstand) + startPosisjon
    in
    yNå


scrollInnBrukerInput : MeldingsLogg -> { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, sisteSpørsmålHeight : Maybe Int } -> Time.Posix -> ( MeldingsLogg, Cmd Msg )
scrollInnBrukerInput meldingsLogg { startTidForScrolling, opprinneligViewport, sisteSpørsmålHeight } tidNå =
    let
        spørsmålHeight =
            case sisteSpørsmålHeight of
                Just height ->
                    -- høyde på innhold + melding-padding + padding-bottom-samtale + margin over melding
                    toFloat (height + (2 * 16) + 16 + 8)

                Nothing ->
                    0

        sluttPosisjonHvisManScrollerHelt =
            opprinneligViewport.scene.height - opprinneligViewport.viewport.height

        lengsteManBurdeScrolle =
            opprinneligViewport.viewport.y + opprinneligViewport.viewport.height - spørsmålHeight

        sluttPosisjon =
            min sluttPosisjonHvisManScrollerHelt lengsteManBurdeScrolle

        scrollTilPosisjon =
            scrollPosition
                { animasjonstidMs = 150
                , opprinneligViewport = opprinneligViewport
                , sluttPosisjon = sluttPosisjon
                , tidNå = tidNå
                , startTidForScrolling = startTidForScrolling
                }
    in
    if scrollTilPosisjon >= (sluttPosisjon - 4) then
        ( MeldingsLogg.avsluttScrollingTilInput meldingsLogg
        , Dom.setViewportOf "samtale" 0 scrollTilPosisjon
            |> Task.attempt ScrolletViewport
        )

    else
        ( meldingsLogg
        , Dom.setViewportOf "samtale" 0 scrollTilPosisjon
            |> Task.attempt ScrolletViewport
        )


subscriptions : MeldingsLogg -> Sub Msg
subscriptions meldingsLogg =
    case MeldingsLogg.scrollAnimasjonStatus meldingsLogg of
        IngenScrollAnimasjon ->
            Sub.none

        ScrollerInnSkriveIndikator _ ->
            Browser.Events.onAnimationFrame AnimationFrame

        ScrollerInnMelding _ ->
            Browser.Events.onAnimationFrame AnimationFrame

        ScrollerInnInputFelt _ ->
            Browser.Events.onAnimationFrame AnimationFrame
