port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode
import Model exposing (MainModel)
import Random exposing (..)
import Single exposing (..)
import Time exposing (..)
import Url exposing (..)


main : Program () Model Msg
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
    Model.MainModel


init : () -> ( Model, Cmd Msg )
init _ =
    ( { targetChar = ""
      , charSet = Array.fromList <| Single.numSet ++ Single.alphaSet
      , spend = 0
      , charLength = 20
      , mode = "single"
      , viewStatus = "title"
      , isNum = True
      , isAlpha = True
      , matchTicker = ""
      , playerOneLeftChar = ""
      , playerTwoLeftChar = ""
      , pid = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | ChangeAtTitle String
    | ChangeAtMulti String
      -- title
    | CheckSingleMode Bool
    | CheckMultiMode Bool
    | SelectMode String
      -- single
    | CheckIsNum Bool
    | CheckIsAlpha Bool
    | SetChar Int
    | Spend Time.Posix
    | SetCharLength String
    | Start
    | End
    | Retry
      -- multi
      -- | Pairing
    | SendMessage String
    | ReceiveMessage String
    | InputMessage String
    | NowMatching Time.Posix
    | Matching
    | Compete
    | SendType
    | Pairing
    | ReceiveType
    | Result
    | EndMulti
    | MultiReady String
    | MultiStart String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change string ->
            let
                position =
                    string
            in
            if model.viewStatus == "title" then
                update (ChangeAtTitle string) model

            else if model.viewStatus == "running" then
                update End
                    { model
                        | targetChar = removeChar model position
                    }

            else if model.viewStatus == "battle" then
                update (ChangeAtMulti string) model

            else
                ( model, Cmd.none )

        ChangeAtTitle string ->
            if string == "Enter" then
                update (SelectMode model.mode) model

            else
                ( model, Cmd.none )

        ChangeAtMulti string ->
            let
                afterChar =
                    removeMultiChar model string
            in
            if model.playerOneLeftChar == afterChar then
                ( model, Cmd.none )

            else
                update (SendMessage afterChar) model

        CheckSingleMode b ->
            ( { model | mode = "single" }, Cmd.none )

        CheckMultiMode b ->
            ( { model | mode = "multi" }, Cmd.none )

        SelectMode m ->
            let
                v =
                    if m == "multi" then
                        "multi"

                    else
                        "config"
            in
            if m == "multi" then
                update Pairing { model | viewStatus = "multi", mode = m }

            else
                ( { model | viewStatus = "config", mode = m }, Cmd.none )

        CheckIsNum b ->
            ( Single.checkIsNum b model, Cmd.none )

        CheckIsAlpha b ->
            ( Single.checkIsAlpha b model, Cmd.none )

        SetChar n ->
            if String.length model.targetChar == model.charLength then
                ( Single.setChar n model, Cmd.none )

            else
                let
                    str =
                        Array.get n model.charSet
                in
                case str of
                    Just s ->
                        ( Single.setChar n model, Random.generate SetChar (Random.int 0 <| Array.length model.charSet) )

                    _ ->
                        ( model, Random.generate SetChar (Random.int 0 <| Array.length model.charSet) )

        Spend _ ->
            ( Single.spend model, Cmd.none )

        SetCharLength s ->
            ( Single.setCharLength s model, Cmd.none )

        Start ->
            ( Single.start model, Random.generate SetChar (Random.int 0 <| Array.length model.charSet) )

        End ->
            ( Single.end model, Cmd.none )

        Retry ->
            init ()

        -- multi
        Matching ->
            ( model, Cmd.none )

        Compete ->
            ( model, Cmd.none )

        SendType ->
            ( model, Cmd.none )

        ReceiveType ->
            ( model, Cmd.none )

        Result ->
            ( model, Cmd.none )

        NowMatching _ ->
            let
                ticker =
                    if model.matchTicker == "..." then
                        ""

                    else
                        model.matchTicker ++ "."
            in
            ( { model | matchTicker = ticker }, Cmd.none )

        Pairing ->
            ( model, paringRoom () )

        SendMessage s ->
            if s == "" then
                update EndMulti
                    { model
                        | playerOneLeftChar = removeMultiChar model s
                    }

            else
                ( model, sendMessage s )

        MultiReady s ->
            ( { model | viewStatus = "battle", targetChar = s, playerOneLeftChar = s, playerTwoLeftChar = s }, Cmd.none )

        MultiStart s ->
            ( { model | viewStatus = "battle", targetChar = s, playerOneLeftChar = s, playerTwoLeftChar = s }, Cmd.none )

        ReceiveMessage s ->
            let
                message =
                    Decode.decodeString (Decode.at [ "message" ] Decode.string) s

                targetChar =
                    Decode.decodeString (Decode.at [ "targetChar" ] Decode.string) s

                p1char =
                    case Decode.decodeString (Decode.at [ "playerOneLeftChar" ] Decode.string) s of
                        Ok tchr ->
                            tchr

                        _ ->
                            model.playerOneLeftChar

                p2char =
                    case Decode.decodeString (Decode.at [ "playerTwoLeftChar" ] Decode.string) s of
                        Ok tchr ->
                            tchr

                        _ ->
                            model.playerTwoLeftChar

                p1id =
                    case Decode.decodeString (Decode.at [ "p1id" ] Decode.string) s of
                        Ok tchr ->
                            Just tchr

                        _ ->
                            model.pid

                p2id =
                    case Decode.decodeString (Decode.at [ "p2id" ] Decode.string) s of
                        Ok tchr ->
                            Just tchr

                        _ ->
                            model.pid

                m =
                    if model.pid == Nothing then
                        { model | pid = p2id }

                    else
                        model

                a =
                    Debug.log "all of message" s

                b =
                    Debug.log "all of message" model
            in
            case message of
                Ok res ->
                    if res == "wait" then
                        ( { model | pid = p1id }, Cmd.none )

                    else if res == "pairing" then
                        case targetChar of
                            Ok tchr ->
                                update (MultiReady tchr) m

                            _ ->
                                ( model, Cmd.none )

                    else if res == "typed" then
                        ( { model | playerOneLeftChar = p1char, playerTwoLeftChar = p2char }, Cmd.none )

                    else
                        -- ( { model | receivedMessage = s }, Cmd.none )
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        InputMessage s ->
            -- ( { model | inputMessage = s }, Cmd.none )
            ( model, Cmd.none )

        EndMulti ->
            ( Single.end model, Cmd.none )


removeChar : Model -> String -> String
removeChar model addChar =
    let
        s =
            String.left 1 model.targetChar
    in
    case s == addChar of
        True ->
            case String.uncons model.targetChar of
                Just ( h, tl ) ->
                    tl

                _ ->
                    ""

        _ ->
            model.targetChar


removeMultiChar : Model -> String -> String
removeMultiChar model addChar =
    let
        s =
            String.left 1 model.playerOneLeftChar
    in
    case s == addChar of
        True ->
            case String.uncons model.playerOneLeftChar of
                Just ( h, tl ) ->
                    tl

                _ ->
                    ""

        _ ->
            model.playerOneLeftChar



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "content" ]
        [ div
            [ class "inner", class <| toggleClass model.viewStatus "title" ]
            [ div [ class "title" ]
                [ h1 []
                    [ text "Typing Game" ]
                , p []
                    [ text "Press Enter Key" ]
                , label
                    [ class "modeLabel" ]
                    [ input [ type_ "radio", class "nes-checkbox is-dark", checked (model.mode == "single"), onCheck CheckSingleMode ] []
                    , span [] [ text "SINGLE" ]
                    ]
                , label
                    [ class "modeLabel" ]
                    [ input [ type_ "radio", class "nes-checkbox is-dark", checked (model.mode == "multi"), onCheck CheckMultiMode ] []
                    , span [] [ text "MULTI\u{3000}" ]
                    ]

                -- , label [ class "modeLabel" ]
                --     [ input [ type_ "radio", class "nes-checkbox is-dark", checked (model.mode == "option"), onCheck CheckMultiMode ] []
                --     , span [] [ text "OPTION\u{3000}" ]
                --     ]
                ]
            ]

        -- single
        , div [ class "inner", class <| toggleClass model.viewStatus "config" ]
            [ h1 [] [ text "set char length" ]
            , input [ class "char-length nes-input is-dark", type_ "number", Html.Attributes.max "9999", Html.Attributes.min "1", placeholder "1 - 9999", value <| String.fromInt model.charLength, onInput SetCharLength ] []
            , input [ class "nes-btn", type_ "button", value "Go!", onClick Start ] []
            , div [ style "display" "block" ]
                [ label []
                    [ input [ type_ "checkbox", class "nes-checkbox is-dark", checked model.isNum, onCheck CheckIsNum ] []
                    , span [] [ text "Num " ]
                    ]
                , label []
                    [ input [ type_ "checkbox", class "nes-checkbox is-dark", checked model.isAlpha, onCheck CheckIsAlpha ] []
                    , span [] [ text " Alpha" ]
                    ]
                ]
            ]
        , div [ class "inner", class <| toggleClass model.viewStatus "running" ]
            [ h1 [] [ text "<C-r> 3fd zfj" ]

            -- , span [ class "target-char" ] [ text model.targetChar ]
            -- , input [ class "target-char", value model.targetChar, (property "selectionStart" (Json.Encode.int 0))] []
            , input [ class "ghost char-length nes-input is-dark", value model.targetChar, style "caret-color" "transparent" ] []
            , input [ class "real char-length nes-input is-dark", value model.targetChar, style "caret-color" "transparent", attribute "disabled" "" ] []
            , h1 [] [ text (timeStringFromMs model.spend) ]
            ]
        , div [ class "inner", class <| toggleClass model.viewStatus "result" ]
            [ h1 [] [ text "<C-r> 3fd zfj" ]
            , span [ class "target-char" ] [ text "the end." ]
            , h1 [ class "time" ] [ text (timeStringFromMs model.spend) ]
            , div []
                [ input [ class "nes-btn retry", type_ "button", value "retry", onClick Retry ] []
                , a [ target "_blank", href <| makeShareUrl model ] [ i [ class "nes-icon twitter is-large" ] [] ]
                ]
            ]

        -- multi
        , div [ class "inner", class <| toggleClass model.viewStatus "multi" ]
            [ h1 [ class "matchTicker" ] [ text <| "Now Matching" ++ model.matchTicker ]
            ]
        , div [ class "inner", class <| toggleClass model.viewStatus "battle" ]
            [ h1 [] [ text "You are vimmer, you are vimmer!" ]
            , h1 [] [ text (timeStringFromMs model.spend) ]
            , input [ class "ghost char-length nes-input is-dark", value model.playerOneLeftChar, style "caret-color" "transparent" ] []
            , input [ class "real char-length nes-input is-dark", value model.playerOneLeftChar, style "caret-color" "transparent", attribute "disabled" "" ] []
            , h1 [] [ text "" ]
            , input [ class "real char-length nes-input is-dark", value model.playerTwoLeftChar, style "caret-color" "transparent", attribute "disabled" "" ] []
            ]
        ]


makeShareUrl : Model -> String
makeShareUrl model =
    "https://twitter.com/intent/tweet?text=" ++ (Url.percentEncode <| String.fromInt model.charLength ++ "文字を" ++ timeStringFromMs model.spend ++ "秒で打ち込んだ | https://qsk.netlify.com/")


toggleClass : String -> String -> String
toggleClass status className =
    if status == className then
        "show"

    else
        "hide"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Change (Decode.field "key" Decode.string))
        , Time.every 10 Spend
        , Time.every 1000 NowMatching
        , receiveMessage ReceiveMessage
        ]


timeStringFromMs : Float -> String
timeStringFromMs ms =
    let
        centiSeconds =
            truncate <| toFloat <| remainderBy 100 <| round ms

        seconds =
            truncate (ms / 100)

        minutes =
            seconds // 60

        hours =
            minutes // 60

        secondsMod =
            remainderBy 60 seconds

        minutesMod =
            minutes

        centiString =
            centiSeconds |> String.fromInt |> String.padLeft 2 '0'

        secondsString =
            secondsMod |> String.fromInt |> String.padLeft 2 '0'

        minutesString =
            minutesMod |> String.fromInt |> String.padLeft 2 '0'
    in
    minutesString ++ ":" ++ secondsString ++ "." ++ centiString
