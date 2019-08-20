module Model exposing (MainModel)

import Array exposing (Array)


type alias MainModel =
    { targetChar : String
    , charSet : Array.Array String
    , spend : Float
    , charLength : Int
    , mode : String
    , viewStatus : String
    , isNum : Bool
    , isAlpha : Bool
    , matchTicker : String
    , playerOneLeftChar : String
    , playerTwoLeftChar : String
    , p1id : Maybe String
    , p2id : Maybe String
    , pid : Maybe String
    , message : String
    , action : String
    , selected : String
    , missed : Bool
    }
