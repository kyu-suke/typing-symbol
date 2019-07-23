port module WebSocket exposing (Model, Msg(..), init, main, paringRoom, receiveMessage, sendMessage, subscriptions, update, view)

import Browser exposing (element)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Encode


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


port paringRoom : () -> Cmd msg


port sendMessage : String -> Cmd msg


port receiveMessage : (String -> msg) -> Sub msg



-- MODEL


type alias Model =
    { dieFace : Int
    , receivedMessage : String
    , inputMessage : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 1 "AAAAAAAAA" "", Cmd.none )



-- UPDATE


type Msg
    = Pairing
    | SendMessage
    | ReceiveMessage String
    | InputMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Pairing ->
            ( model, paringRoom () )

        SendMessage ->
            ( model, sendMessage model.inputMessage )

        ReceiveMessage s ->
            ( { model | receivedMessage = s }, Cmd.none )

        InputMessage s ->
            ( { model | inputMessage = s }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    receiveMessage ReceiveMessage



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text (String.fromInt model.dieFace) ]
        , h1 [] [ text model.receivedMessage ]
        , h1 [] [ text model.inputMessage ]
        , button [ onClick Pairing ] [ text "Pairing" ]
        , input [ type_ "text", value model.inputMessage, onInput InputMessage ] []
        , button [ onClick SendMessage ] [ text "send" ]
        ]
