module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Random exposing (..)


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
    { char : Char
    , targetChar : Char
    }


type alias Char =
    String


type alias TypeSymbol =
    String


type alias DoneSymbol =
    String


type alias Row =
    Array Cell


type Cell
    = Tile Int
    | Empty


type alias Position =
    ( Int, Int )


type Direction
    = Left
    | Right
    | Up
    | Down
    | Other


charSet =
    [ "!", "\"", "\\", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", "0", "1", "2", "3", "4", "5", "6", "7", "8", "9", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "_", "`", "{", "|", "}", "~", "¥" ]


init : () -> ( Model, Cmd Msg )
init _ =
    ( { char =
            "a"
      , targetChar = String.join "" <| charSet ++ charSet ++ charSet ++ charSet
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Change String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Change string ->
            let
                position =
                    string
            in
            ( { model
                | char = setChar model position
                , targetChar = removeChar model position
              }
            , Cmd.none
            )


setChar : Model -> String -> Char
setChar model addChar =
    let
        s =
            String.left 1 model.targetChar
    in
    case s == addChar of
        True ->
            model.char ++ addChar

        _ ->
            model.char


removeChar : Model -> String -> Char
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


i =
    Random.step (int 0 10) (initialSeed 2)



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ style "width" "100%"
        , style "overflow" "hidden"
        ]
        [ h1 [] [ text "Hello" ]
        , h1 [] [ text <| String.fromInt <| Tuple.first i ]
        , span [ id "pre" ] [ text <| List.foldl (++) "" charSet ]
        , span [ id "af" ] [ text <| List.foldl (++) "" charSet ]
        , h1 [] [ text model.char ]
        , h1 [ style "width" "500%" ] [ text model.targetChar ]
        ]


viewRow : Row -> Html Msg
viewRow row =
    div [] <|
        Array.toList (Array.map viewCell row)


viewCell : Cell -> Html Msg
viewCell cell =
    case cell of
        Tile number ->
            span []
                [ text (String.fromInt number)
                ]

        Empty ->
            span [] []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map Change keyDecoder)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.map toDirection (Decode.field "key" Decode.string)


toDirection : String -> String
toDirection string =
    string
