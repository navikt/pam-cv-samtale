module SamtaleAnimasjon exposing (scrollTilBunn)

import Browser.Dom as Dom
import Task


scrollTilBunn : (Result Dom.Error () -> msg) -> Cmd msg
scrollTilBunn msgKonstruktør =
    Dom.getViewportOf "samtale"
        |> Task.andThen (\viewportInfo -> Dom.setViewportOf "samtale" 0 (viewportInfo.scene.height - viewportInfo.viewport.height))
        |> Task.attempt msgKonstruktør
