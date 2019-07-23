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
    }
