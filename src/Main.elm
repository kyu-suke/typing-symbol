module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as Decode
import Json.Encode
import Random exposing (..)
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



-- MODEL


type alias Model =
    { targetChar : String
    , charSet : Array.Array String
    , spend : Float
    , charLength : Int
    , mode : String
    , titleShow : String
    , configShow : String
    , runningShow : String
    , resultShow : String
    , isNum : Bool
    , isAlpha : Bool
    }



-- charSet =
--     Array.fromList
--         [ "!", "\"", "\\", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "_", "`", "{", "|", "}", "~", "¥" ]


numSet =
    [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]


alphaSet =
    [ "!", "\"", "\\", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "_", "`", "{", "|", "}", "~", "¥" ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { targetChar = ""
      , charSet = Array.fromList <| numSet ++ alphaSet
      , spend = 0
      , charLength = 20
      , mode = "single"
      , titleShow = "show"
      , configShow = "hide"
      , runningShow = "hide"
      , resultShow = "hide"
      , isNum = True
      , isAlpha = True
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String
    | ChangeCharSet
    | SetChar Int
    | Spend Time.Posix
    | SetCharLength String
    | CheckIsNum Bool
    | CheckIsAlpha Bool
    | CheckSingleMode Bool
    | CheckMultiMode Bool
    | SelectMode String
    | Start
    | End
    | Retry


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change string ->
            let
                position =
                    string
            in
            if model.titleShow == "show" && string == "Enter" then
                update (SelectMode model.mode) model

            else
                update End
                    { model
                        | targetChar = removeChar model position
                    }

        ChangeCharSet ->
            let
                charSet =
                    Array.fromList <|
                        (if model.isAlpha then
                            alphaSet

                         else
                            []
                        )
                            ++ (if model.isNum then
                                    numSet

                                else
                                    []
                               )
            in
            ( { model | charSet = charSet }, Cmd.none )

        SetChar newFace ->
            if String.length model.targetChar == model.charLength then
                ( model, Cmd.none )

            else
                let
                    str =
                        Array.get newFace model.charSet

                    _ =
                        Debug.log "modelをprintして、返り値はmodel" model.charSet

                    _ =
                        Debug.log "modelをprintして、返り値はmodel" str
                in
                case str of
                    Just s ->
                        ( { model | targetChar = model.targetChar ++ s }, Random.generate SetChar (Random.int 0 <| Array.length model.charSet) )

                    _ ->
                        ( model, Random.generate SetChar (Random.int 0 <| Array.length model.charSet) )

        Spend _ ->
            if model.runningShow == "show" then
                ( { model | spend = model.spend + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

        SetCharLength cl ->
            case String.toInt cl of
                Just i ->
                    if i < 1 then
                        ( model, Cmd.none )

                    else if i > 9999 then
                        ( model, Cmd.none )

                    else
                        ( { model | charLength = i }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        CheckIsNum b ->
            update ChangeCharSet { model | isNum = b }

        CheckIsAlpha b ->
            update ChangeCharSet { model | isAlpha = b }

        CheckSingleMode b ->
            ( { model | mode = "single" }, Cmd.none )

        CheckMultiMode b ->
            ( { model | mode = "multi" }, Cmd.none )

        SelectMode m ->
            ( { model | titleShow = "hide", configShow = "show", mode = m }, Cmd.none )

        Start ->
            ( { model | configShow = "hide", runningShow = "show" }, Random.generate SetChar (Random.int 0 <| Array.length model.charSet) )

        End ->
            if String.length model.targetChar <= 0 && model.runningShow == "show" then
                ( { model | runningShow = "hide", resultShow = "show" }, Cmd.none )

            else
                ( model, Cmd.none )

        Retry ->
            init ()


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



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ class "content" ]
        [ div [ class "inner", class model.titleShow ]
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
                ]
            ]
        , div [ class "inner", class model.configShow ]
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
        , div [ class "inner", class model.runningShow ]
            [ h1 [] [ text "<C-r> 3fd zfj" ]

            -- , span [ class "target-char" ] [ text model.targetChar ]
            -- , input [ class "target-char", value model.targetChar, (property "selectionStart" (Json.Encode.int 0))] []
            , input [ class "ghost char-length nes-input is-dark", value model.targetChar, style "caret-color" "transparent" ] []
            , input [ class "real char-length nes-input is-dark", value model.targetChar, style "caret-color" "transparent", attribute "disabled" "" ] []
            , h1 [] [ text (timeStringFromMs model.spend) ]
            ]
        , div [ class "inner", class model.resultShow ]
            [ h1 [] [ text "<C-r> 3fd zfj" ]
            , span [ class "target-char" ] [ text "the end." ]
            , h1 [ class "time" ] [ text (timeStringFromMs model.spend) ]
            , div []
                [ input [ class "nes-btn retry", type_ "button", value "retry", onClick Retry ] []
                , a [ target "_blank", href <| makeShareUrl model ] [ i [ class "nes-icon twitter is-large" ] [] ]
                ]
            ]
        ]


makeShareUrl : Model -> String
makeShareUrl model =
    "https://twitter.com/intent/tweet?text=" ++ (Url.percentEncode <| String.fromInt model.charLength ++ "文字を" ++ timeStringFromMs model.spend ++ "秒で打ち込んだ | https://qsk.netlify.com/")



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Change (Decode.field "key" Decode.string))
        , Time.every 10 Spend
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
