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
import String exposing (replace)
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


port closeConnection : () -> Cmd msg


port receiveMessage : (String -> msg) -> Sub msg


port closeConnectionMsg : (String -> msg) -> Sub msg



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
      , p1id = Nothing
      , p2id = Nothing
      , pid = Nothing
      , message = "You are vimmer, you are vimmer!"
      , action = ""
      , selected = "charLength"
      , missed = False
      }
    , closeConnection ()
    )



-- UPDATE


type Msg
    = Change String
    | ChangeAtTitle String
    | ChangeConfig String
    | ChangeAtMulti String
      -- title
    | CheckSingleMode Bool
    | CheckMultiMode Bool
    | SelectMode String
      -- single
    | CheckIsNum Bool
    | CheckIsAlpha Bool
    | SetChar Int
    | MissReset Time.Posix
    | Spend Time.Posix
    | SetCharLength String
    | Start
    | End
    | Retry
      -- multi
      -- | Pairing
    | MultiSpend Time.Posix
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
    | Typed String String
    | MultiReady String
    | MultiStart String
    | CloseConnection String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change string ->
            if model.viewStatus == "title" then
                update (ChangeAtTitle string) model

            else if model.viewStatus == "running" then
                let
                    char =
                        removeChar model string

                    missed =
                        if string == "Control" || string == "Shift" || string == "Alt" || string == "Meta" || string == "Shift" || string == "Meta" then
                            False

                        else if model.targetChar == char then
                            True

                        else
                            False
                in
                update End
                    { model
                        | missed = missed
                        , targetChar = char
                    }

            else if model.viewStatus == "ready" || model.viewStatus == "battle" then
                update (ChangeAtMulti string) model

            else if model.viewStatus == "config" then
                update (ChangeConfig string) model

            else
                ( model, Cmd.none )

        ChangeAtTitle string ->
            if string == "ArrowDown" || string == "ArrowUp" || string == "j" || string == "k" then
                if model.mode == "single" then
                    update (CheckMultiMode True) model

                else
                    update (CheckSingleMode True) model

            else if string == "Enter" then
                update (SelectMode model.mode) model

            else
                ( model, Cmd.none )

        ChangeConfig string ->
            if string == "Backspace" || string == "Delete" || string == "-" then
                if model.selected == "charLength" then
                    ( { model | charLength = model.charLength - 1 }, Cmd.none )

                else if model.selected == "isNum" then
                    update (CheckIsNum (not model.isNum)) model

                else if model.selected == "isAlpha" then
                    update (CheckIsAlpha (not model.isAlpha)) model

                else
                    ( model, Cmd.none )

            else if string == "Enter" || string == "+" then
                if model.selected == "charLength" then
                    ( { model | charLength = model.charLength + 1 }, Cmd.none )

                else if model.selected == "isNum" then
                    update (CheckIsNum (not model.isNum)) model

                else if model.selected == "isAlpha" then
                    update (CheckIsAlpha (not model.isAlpha)) model

                else if model.selected == "start" then
                    update Start model

                else
                    ( model, Cmd.none )

            else if string == "ArrowDown" || string == "j" then
                if model.selected == "charLength" then
                    ( { model | selected = "isNum" }, Cmd.none )

                else if model.selected == "isNum" then
                    ( { model | selected = "isAlpha" }, Cmd.none )

                else if model.selected == "isAlpha" then
                    ( { model | selected = "start" }, Cmd.none )

                else if model.selected == "start" then
                    ( { model | selected = "charLength" }, Cmd.none )

                else
                    ( model, Cmd.none )

            else if string == "ArrowUp" || string == "k" then
                if model.selected == "charLength" then
                    ( { model | selected = "start" }, Cmd.none )

                else if model.selected == "isNum" then
                    ( { model | selected = "charLength" }, Cmd.none )

                else if model.selected == "isAlpha" then
                    ( { model | selected = "isNum" }, Cmd.none )

                else if model.selected == "start" then
                    ( { model | selected = "isAlpha" }, Cmd.none )

                else
                    ( model, Cmd.none )

            else
                ( model, Cmd.none )

        ChangeAtMulti string ->
            let
                leftChar =
                    if model.pid == model.p1id then
                        model.playerOneLeftChar

                    else
                        model.playerTwoLeftChar

                afterChar =
                    removeMultiChar model string
            in
            if leftChar == afterChar then
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

        MissReset _ ->
            ( { model | missed = False }, Cmd.none )

        Spend _ ->
            ( Single.spend model, Cmd.none )

        SetCharLength s ->
            ( Single.setCharLength s model, Cmd.none )

        Start ->
            if model.isNum || model.isAlpha then
                ( Single.start model, Random.generate SetChar (Random.int 0 <| Array.length model.charSet) )

            else
                ( model, Cmd.none )

        End ->
            ( Single.end model, Cmd.none )

        Retry ->
            init ()

        -- multi
        MultiSpend _ ->
            if model.viewStatus == "battle" then
                ( { model | spend = model.spend + 1 }, Cmd.none )

            else
                ( model, Cmd.none )

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
                ( model, sendMessage s )
                --update EndMulti
                --    { model
                --        | playerOneLeftChar = removeMultiChar model s
                --    }

            else
                ( model, sendMessage <| String.replace "\"" "\\\"" <| String.replace "\\" "\\\\" s )

        Typed p1char p2char ->
            --let
            --    ( modelp1char, modelp2char ) =
            --        if model.pid == model.p1id then
            --            ( p1char, p2char )
            --        else
            --            ( p2char, p1char )
            --    a =
            --        Debug.log "p2char:  " p2char
            --in
            ( { model | playerOneLeftChar = p1char, playerTwoLeftChar = p2char }, Cmd.none )

        MultiReady s ->
            ( { model | viewStatus = "ready", targetChar = s, playerOneLeftChar = s, playerTwoLeftChar = s }, Cmd.none )

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
                    case Decode.decodeString (Decode.at [ "p1Id" ] Decode.string) s of
                        Ok tchr ->
                            Just tchr

                        _ ->
                            model.pid

                p2id =
                    case Decode.decodeString (Decode.at [ "p2Id" ] Decode.string) s of
                        Ok tchr ->
                            Just tchr

                        _ ->
                            model.pid

                m =
                    if model.pid == Nothing then
                        { model | pid = p2id, p1id = p1id, p2id = p2id }

                    else
                        { model | p1id = p1id, p2id = p2id }
            in
            if s == "the other one is disconnected!" then
                ( { model | action = "closeConnect" }, closeConnection () )

            else
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
                            update (Typed p1char p2char) m

                        else if res == "battle" then
                            update (Typed p1char p2char) { m | viewStatus = "battle" }

                        else if res == "end" then
                            let
                                ( endModel, c ) =
                                    update (Typed p1char p2char) m
                            in
                            update EndMulti endModel

                        else
                            -- ( { model | receivedMessage = s }, Cmd.none )
                            ( model, Cmd.none )

                    _ ->
                        ( model, Cmd.none )

        InputMessage s ->
            -- ( { model | inputMessage = s }, Cmd.none )
            ( model, Cmd.none )

        EndMulti ->
            let
                leftChar =
                    if model.pid == model.p1id then
                        model.playerOneLeftChar

                    else
                        model.playerTwoLeftChar

                message =
                    if String.length leftChar > 0 then
                        "Lose, but you are Vimmer!"

                    else
                        "Win, yes you are The Vimmer!"
            in
            ( { model | viewStatus = "end", message = message }, Cmd.none )

        CloseConnection _ ->
            ( { model | action = "closeConnect" }, closeConnection () )


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
        chr =
            if model.pid == model.p1id then
                model.playerOneLeftChar

            else
                model.playerTwoLeftChar

        s =
            String.left 1 chr
    in
    case s == addChar of
        True ->
            case String.uncons chr of
                Just ( h, tl ) ->
                    tl

                _ ->
                    ""

        _ ->
            chr



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
            , div [ style "display" "block", class "menuWrap" ]
                [ div [ style "display" "block", selecredMenuClass model "charLength" ]
                    [ input [ class "menu char-length nes-input is-dark", type_ "number", Html.Attributes.max "9999", Html.Attributes.min "1", placeholder "1 - 9999", value <| String.fromInt model.charLength, onInput SetCharLength ] []
                    ]
                , div [ style "display" "block", selecredMenuClass model "isNum" ]
                    [ label []
                        [ input [ type_ "checkbox", class "nes-checkbox is-dark", checked model.isNum, onCheck CheckIsNum ] []
                        , span [] [ text "Num\u{00A0}\u{00A0}" ]
                        ]
                    ]
                , div [ style "display" "block", selecredMenuClass model "isAlpha" ]
                    [ label []
                        [ input [ type_ "checkbox", class "nes-checkbox is-dark", checked model.isAlpha, onCheck CheckIsAlpha ] []
                        , span [] [ text "Alpha" ]
                        ]
                    ]
                , div [ style "display" "block", class "start", selecredMenuClass model "start" ]
                    [ input [ class "nes-btn start", type_ "button", value "Start!", onClick Start ] []
                    ]
                ]
            ]
        , div
            [ class <| toggleClass model.viewStatus "running"
            , class <|
                if model.missed then
                    "inner missed"

                else
                    "inner"
            ]
            [ h1 [] [ text "type, type, type" ]

            -- , span [ class "target-char" ] [ text model.targetChar ]
            -- , input [ class "target-char", value model.targetChar, (property "selectionStart" (Json.Encode.int 0))] []
            , input [ class "ghost char-length nes-input is-dark", value model.targetChar, style "caret-color" "transparent" ] []
            , input [ class "real char-length nes-input is-dark", value model.targetChar, style "caret-color" "transparent", attribute "disabled" "" ] []
            , h1 [] [ text (timeStringFromMs model.spend) ]
            ]
        , div [ class "inner", class <| toggleClass model.viewStatus "result" ]
            [ h1 [] [ text "vim" ]
            , span [ class "theend" ] [ text "the end." ]
            , div [ class "options" ]
                [ h1 [ class "time" ] [ text (timeStringFromMs model.spend) ]
                , input [ class "nes-btn retry", type_ "button", value "retry", onClick Retry ] []
                , a [ class "twitter", target "_blank", href <| makeShareUrl model ] [ i [ class "nes-icon twitter is-medium" ] [] ]
                ]
            ]

        -- multi
        , div [ class "inner", class <| toggleClass model.viewStatus "multi" ]
            [ h1 [ class "matchTicker" ] [ text <| "Now Matching" ++ model.matchTicker ]
            ]
        , div [ class "inner", class <| battleOrEnd model.viewStatus ]
            [ h1 [] [ text model.message ]
            , h1 [] [ text (timeStringFromMs model.spend) ]
            , input [ class "real char-length nes-input is-dark", value model.playerOneLeftChar, style "caret-color" "transparent", attribute "disabled" "" ] []
            , h1 [] [ text "" ]
            , input [ class "real char-length nes-input is-dark", value model.playerTwoLeftChar, style "caret-color" "transparent", attribute "disabled" "" ] []
            , div [ hidden <| model.viewStatus /= "end" ]
                [ input [ class "nes-btn retry-multi", type_ "button", value "retry", onClick Retry ] []
                ]
            ]
        , div [ class "connectionWindow nes-container is-rounded is-dark", hidden <| model.action /= "closeConnect" ]
            [ p [] [ text "Connection is closed." ]
            , p [] [ text "Retry matching." ]
            , p [] [ a [ class "back", onClick Retry ] [ text "back" ] ]
            ]
        ]


makeShareUrl : Model -> String
makeShareUrl model =
    "https://twitter.com/intent/tweet?text=" ++ (Url.percentEncode <| String.fromInt model.charLength ++ "文字を" ++ timeStringFromMs model.spend ++ "秒で打ち込んだ | https://typingame.netlify.com")


toggleClass : String -> String -> String
toggleClass status className =
    if status == className then
        "show"

    else
        "hide"


battleOrEnd : String -> String
battleOrEnd s =
    if toggleClass s "battle" == "show" || toggleClass s "ready" == "show" || toggleClass s "end" == "show" then
        "show"

    else
        "hide"


selecredMenuClass : Model -> String -> Attribute msg
selecredMenuClass m s =
    if m.selected == s then
        class "menu selected"

    else
        class "menu"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Change (Decode.field "key" Decode.string))
        , Time.every 400 MissReset
        , Time.every 10 Spend
        , Time.every 10 MultiSpend
        , Time.every 1000 NowMatching
        , receiveMessage ReceiveMessage
        , closeConnectionMsg CloseConnection
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
