module SamtaleAnimasjon exposing (Msg, startAnimasjon, subscriptions, update)

import Browser.Dom as Dom exposing (Element, Viewport)
import Browser.Events
import DebugStatus exposing (DebugStatus)
import Ease
import Konstanter
import MeldingsLogg exposing (AntallOrdNesteOgForrigeMelding(..), FerdigAnimertStatus(..), MeldingsLogg, ScrollAnimasjonStatus(..))
import Process
import Task exposing (Task)
import Time


type Msg
    = StartÅSkrive (Result Dom.Error ( Viewport, Time.Posix, Element ))
    | StartMeldingsanimasjon
    | RegistrerMeldingsdimensjoner (Result Dom.Error MeldingDimensjonInfo)
    | FullførMeldingsanimasjon
    | VisBrukerInput
    | StartÅScrolleInnInput (Result Dom.Error ( Viewport, Time.Posix ))
    | AnimationFrame Time.Posix
    | ScrolletViewport (Result Dom.Error ())


type alias MeldingDimensjonInfo =
    { melding : Element
    , viewport : Viewport
    , timestamp : Time.Posix
    , samtaleElement : Element
    }


startAnimasjon : DebugStatus -> Cmd Msg
startAnimasjon debugStatus =
    500
        |> DebugStatus.meldingsTimeout debugStatus
        |> Process.sleep
        |> Task.andThen (\_ -> getTimeViewportAndElement)
        |> Task.attempt StartÅSkrive


update : DebugStatus -> Msg -> MeldingsLogg -> ( MeldingsLogg, Cmd Msg )
update debugStatus msg meldingsLogg =
    case msg of
        StartÅSkrive result ->
            case result of
                Ok ( viewport, posix, samtaleElement ) ->
                    if DebugStatus.hoppOverMeldingsanimasjon debugStatus then
                        hoppOverMeldingsanimasjon meldingsLogg

                    else
                        let
                            nyMeldingslogg =
                                MeldingsLogg.startÅSkrive posix viewport samtaleElement meldingsLogg
                        in
                        ( nyMeldingslogg
                        , nyMeldingslogg
                            |> lengdePåSkriveindikatorIMillisekunder
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
                Ok { melding, viewport, timestamp, samtaleElement } ->
                    ( meldingsLogg
                        |> MeldingsLogg.registrerDimensjoner
                            { height = ceiling melding.element.height
                            , width = ceiling melding.element.width
                            , viewport = viewport
                            , posix = timestamp
                            , samtaleElement = samtaleElement
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
            if MeldingsLogg.alleMeldingerVises nyMeldingslogg then
                ( nyMeldingslogg
                , meldingsLogg
                    |> ventetidFørBrukerinputScrollesInn
                    |> DebugStatus.meldingsTimeout debugStatus
                    |> Process.sleep
                    |> Task.attempt (always VisBrukerInput)
                )

            else
                ( nyMeldingslogg
                , 500
                    |> DebugStatus.meldingsTimeout debugStatus
                    |> Process.sleep
                    |> Task.andThen (\_ -> getTimeViewportAndElement)
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
                    if (viewport.scene.height - (viewport.viewport.y + viewport.viewport.height)) >= 70 then
                        ( MeldingsLogg.startScrollingTilInput timestamp viewport meldingsLogg, Cmd.none )

                    else
                        ( meldingsLogg
                            |> MeldingsLogg.startScrollingTilInput timestamp viewport
                            |> MeldingsLogg.avsluttScrollingTilInput
                        , Cmd.none
                        )

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


lengdePåSkriveindikatorIMillisekunder : MeldingsLogg -> Float
lengdePåSkriveindikatorIMillisekunder meldingsLogg =
    case MeldingsLogg.antallOrdForrigeOgNesteMelding meldingsLogg of
        AlleMeldingerAnimert ->
            400

        FørsteMelding antallOrdNesteMelding ->
            1600

        FinnesEnForrigeMelding { forrige, neste } ->
            let
                gjennomsnitteligAntallOrd =
                    (((forrige * 2) + neste) // 3)
                        |> toFloat
            in
            gjennomsnitteligAntallOrd * 200


ventetidFørBrukerinputScrollesInn : MeldingsLogg -> Float
ventetidFørBrukerinputScrollesInn meldingsLogg =
    case MeldingsLogg.antallOrdForrigeOgNesteMelding meldingsLogg of
        AlleMeldingerAnimert ->
            500

        FørsteMelding antallOrdNesteMelding ->
            (antallOrdNesteMelding * 150)
                |> toFloat
                |> clamp 500 3600

        FinnesEnForrigeMelding { neste } ->
            (neste * 150)
                |> toFloat
                |> clamp 500 3600



--- DOM TASKS ---


tilMeldingDimensjoner : Element -> Viewport -> Time.Posix -> Element -> MeldingDimensjonInfo
tilMeldingDimensjoner melding viewport timestamp samtaleElement =
    { melding = melding
    , viewport = viewport
    , timestamp = timestamp
    , samtaleElement = samtaleElement
    }


getDimensionsTimeAndViewport : String -> Task Dom.Error MeldingDimensjonInfo
getDimensionsTimeAndViewport id =
    Task.map4
        tilMeldingDimensjoner
        (Dom.getElement id)
        (Dom.getViewportOf "samtale")
        Time.now
        (Dom.getElement "samtale-innhold")


getTimeAndViewport : Task Dom.Error ( Viewport, Time.Posix )
getTimeAndViewport =
    Task.map2
        Tuple.pair
        (Dom.getViewportOf "samtale")
        Time.now


triple : a -> b -> c -> ( a, b, c )
triple a b c =
    ( a, b, c )


getTimeViewportAndElement : Task Dom.Error ( Viewport, Time.Posix, Element )
getTimeViewportAndElement =
    Task.map3
        triple
        (Dom.getViewportOf "samtale")
        Time.now
        (Dom.getElement "samtale-innhold")



--- SCROLLING ---


scrollTilSkriveIndikator : MeldingsLogg -> { startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, samtaleElement : Element } -> Time.Posix -> ( MeldingsLogg, Cmd Msg )
scrollTilSkriveIndikator meldingsLogg { startTidForScrolling, opprinneligViewport, samtaleElement } tidNå =
    let
        sluttPosisjon =
            samtaleElement.element.height + 40 - 16 - opprinneligViewport.viewport.height
    in
    if sluttPosisjon < 0 then
        ( meldingsLogg, Cmd.none )

    else
        ( meldingsLogg
        , { animasjonstidMs = 400
          , opprinneligViewport = opprinneligViewport
          , sluttPosisjon = sluttPosisjon
          , tidNå = tidNå
          , startTidForScrolling = startTidForScrolling
          }
            |> scrollPositionMeldinger
            |> Dom.setViewportOf "samtale" 0
            |> Task.attempt ScrolletViewport
        )


scrollTilMelding : MeldingsLogg -> { height : Int, startTidForScrolling : Time.Posix, opprinneligViewport : Viewport, samtaleElement : Element } -> Time.Posix -> ( MeldingsLogg, Cmd Msg )
scrollTilMelding meldingsLogg { height, startTidForScrolling, opprinneligViewport, samtaleElement } tidNå =
    let
        forskjellMeldingstørrelse =
            Konstanter.meldingHøyde height - Konstanter.skriveIndikatorHøyde

        sluttPosisjon =
            samtaleElement.element.height + 40 - 16 - opprinneligViewport.viewport.height + toFloat forskjellMeldingstørrelse
    in
    if sluttPosisjon < 0 then
        ( meldingsLogg, Cmd.none )

    else
        ( meldingsLogg
        , { animasjonstidMs = 400
          , opprinneligViewport = opprinneligViewport
          , sluttPosisjon = sluttPosisjon
          , tidNå = tidNå
          , startTidForScrolling = startTidForScrolling
          }
            |> scrollPositionMeldinger
            |> Dom.setViewportOf "samtale" 0
            |> Task.attempt ScrolletViewport
        )


scrollPositionMeldinger :
    { animasjonstidMs : Int
    , opprinneligViewport : Viewport
    , sluttPosisjon : Float
    , tidNå : Time.Posix
    , startTidForScrolling : Time.Posix
    }
    -> Float
scrollPositionMeldinger =
    scrollPosition { easingFunction = Ease.bezier 0.25 0.1 0.25 1.0 }


scrollPosition :
    { easingFunction : Float -> Float }
    ->
        { animasjonstidMs : Int
        , opprinneligViewport : Viewport
        , sluttPosisjon : Float
        , tidNå : Time.Posix
        , startTidForScrolling : Time.Posix
        }
    -> Float
scrollPosition { easingFunction } { animasjonstidMs, opprinneligViewport, sluttPosisjon, tidNå, startTidForScrolling } =
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
            easingFunction andelUtførtTid

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
                    -- høyde på melding + padding-bottom-samtale
                    toFloat (Konstanter.meldingHøyde height + Konstanter.meldingMarginTop + 16)

                Nothing ->
                    0

        sluttPosisjonHvisManScrollerHelt =
            opprinneligViewport.scene.height - opprinneligViewport.viewport.height - 24

        lengsteManBurdeScrolle =
            opprinneligViewport.viewport.y + opprinneligViewport.viewport.height - spørsmålHeight

        sluttPosisjon =
            min sluttPosisjonHvisManScrollerHelt lengsteManBurdeScrolle

        avstand =
            sluttPosisjon - opprinneligViewport.viewport.y

        animasjonsTid =
            (avstand * 2)
                |> round
                |> clamp 200 400

        scrollTilPosisjon =
            scrollPosition
                { easingFunction = Ease.bezier 0 0 0.58 1 }
                { animasjonstidMs = animasjonsTid
                , opprinneligViewport = opprinneligViewport
                , sluttPosisjon = sluttPosisjon
                , tidNå = tidNå
                , startTidForScrolling = startTidForScrolling
                }
    in
    if opprinneligViewport.scene.height - (opprinneligViewport.viewport.y + opprinneligViewport.viewport.height) < 70 then
        ( meldingsLogg, Cmd.none )

    else if scrollTilPosisjon >= (sluttPosisjon - 4) then
        ( MeldingsLogg.avsluttScrollingTilInput meldingsLogg
        , Dom.setViewportOf "samtale" 0 scrollTilPosisjon
            |> Task.attempt ScrolletViewport
        )

    else
        ( meldingsLogg
        , Dom.setViewportOf "samtale" 0 scrollTilPosisjon
            |> Task.attempt ScrolletViewport
        )



--- DEBUG MODUS ---


hoppOverMeldingsanimasjon : MeldingsLogg -> ( MeldingsLogg, Cmd Msg )
hoppOverMeldingsanimasjon meldingsLogg =
    ( MeldingsLogg.debugFullførAlleMeldinger meldingsLogg, scrollTilBunn ScrolletViewport )


scrollTilBunn : (Result Dom.Error () -> msg) -> Cmd msg
scrollTilBunn msgKonstruktør =
    Dom.getViewportOf "samtale"
        |> Task.andThen (\viewportInfo -> Dom.setViewportOf "samtale" 0 (viewportInfo.scene.height - viewportInfo.viewport.height))
        |> Task.attempt msgKonstruktør



--- SUBSCRIPTIONS ---


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
