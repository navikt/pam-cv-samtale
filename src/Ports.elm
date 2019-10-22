port module Ports exposing (MsgFromJavascript(..), fromJavascript)

import Json.Decode exposing (Decoder, fail, succeed)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode


port fromJavascriptToElm : (Json.Encode.Value -> msg) -> Sub msg


type MsgFromJavascript
    = WindowInFocus


fromJavascript : (Result Json.Decode.Error MsgFromJavascript -> msg) -> Sub msg
fromJavascript msg =
    fromJavascriptToElm
        (\jsonValue ->
            jsonValue
                |> decodeMessageFromJavascript
                |> msg
        )


decodeMessageFromJavascript : Json.Encode.Value -> Result Json.Decode.Error MsgFromJavascript
decodeMessageFromJavascript value =
    Json.Decode.decodeValue decodeMsgFromJavascript value


decodeMsgFromJavascript : Decoder MsgFromJavascript
decodeMsgFromJavascript =
    decodeJsonObject
        |> Json.Decode.andThen decodeToMsg


decodeToMsg : JsonObjectWithType -> Decoder MsgFromJavascript
decodeToMsg jsonObject =
    if jsonObject.type_ == "WINDOW_IN_FOCUS" then
        succeed WindowInFocus

    else
        fail ("Klarer ikke decode beskjed fra javascript med type: \"" ++ jsonObject.type_ ++ "\"")


decodeJsonObject : Decoder JsonObjectWithType
decodeJsonObject =
    succeed JsonObjectWithType
        |> required "type" Json.Decode.string
        |> optional "payload" Json.Decode.value Json.Encode.null


type alias JsonObjectWithType =
    { type_ : String
    , payload : Json.Encode.Value
    }
