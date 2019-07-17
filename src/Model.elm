module Model exposing (MainModel)

import Array exposing (Array)


type alias MainModel =
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
