module Single exposing (Model, alphaSet, changeCharSet, checkIsAlpha, checkIsNum, end, numSet, setChar, setCharLength, spend, start)

-- import Single exposing (..)

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
import Time exposing (..)
import Url exposing (..)


alphaSet =
    [ "!", "\"", "\\", "#", "$", "%", "&", "'", "(", ")", "*", "+", ",", "-", ".", "/", ":", ";", "<", "=", ">", "?", "@", "[", "]", "^", "_", "`", "{", "|", "}", "~", "¥" ]


numSet =
    [ "0", "1", "2", "3", "4", "5", "6", "7", "8", "9" ]



-- MODEL


type alias Model =
    Model.MainModel


checkIsNum : Bool -> Model -> Model
checkIsNum b m =
    changeCharSet { m | isNum = b }


checkIsAlpha : Bool -> Model -> Model
checkIsAlpha b m =
    changeCharSet { m | isAlpha = b }


changeCharSet : Model -> Model
changeCharSet m =
    let
        charSet =
            Array.fromList <|
                (if m.isAlpha then
                    alphaSet

                 else
                    []
                )
                    ++ (if m.isNum then
                            numSet

                        else
                            []
                       )
    in
    { m | charSet = charSet }


setChar : Int -> Model -> Model
setChar n m =
    if String.length m.targetChar == m.charLength then
        m

    else
        let
            str =
                Array.get n m.charSet
        in
        case str of
            Just s ->
                { m | targetChar = m.targetChar ++ s }

            _ ->
                m


spend : Model -> Model
spend m =
    if m.viewStatus == "running" then
        { m | spend = m.spend + 1 }

    else
        m


setCharLength : String -> Model -> Model
setCharLength s m =
    case String.toInt s of
        Just i ->
            if i < 1 then
                m

            else if i > 9999 then
                m

            else
                { m | charLength = i }

        Nothing ->
            m


start : Model -> Model
start m =
    { m | viewStatus = "running" }


end : Model -> Model
end m =
    if String.length m.targetChar <= 0 && m.viewStatus == "running" then
        { m | viewStatus = "result" }

    else
        m
