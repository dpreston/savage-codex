module Savage.Data exposing (..)

import Json.Decode as JD exposing (Decoder)
import Json.Decode.Pipeline as JP


type Category
    = Character
    | Power
    | World
    | Invalid


type alias Record =
    { id : Int
    , category : Category
    , title : String
    , firstPage : Int
    , entries : List Entry
    }


type alias Entry =
    { id : Int
    , recordID : Int
    , chapter : Int
    , text : String
    , summary : Maybe String
    }


type alias JsonRoot =
    { records : List Record
    , entries : List Entry
    , maxPage : Int
    }



--


emptyRecord : Record
emptyRecord =
    Record 0 Invalid "no record" 1 []


emptyEntry : Entry
emptyEntry =
    Entry 0 0 0 "no entry" Nothing



-- JSON Decoders


dataDecoder : Decoder JsonRoot
dataDecoder =
    JP.decode JsonRoot
        |> JP.required "records" (JD.list recordDecoder)
        |> JP.required "entries" (JD.list entryDecoder)
        |> JP.required "maxPage" JD.int


recordDecoder : Decoder Record
recordDecoder =
    JP.decode Record
        |> JP.required "id" JD.int
        |> JP.required "category" (JD.string |> JD.andThen categoryDecoder)
        |> JP.required "title" JD.string
        |> JP.required "firstPage" JD.int
        |> JP.required "entries" (JD.list entryDecoder)


entryDecoder : Decoder Entry
entryDecoder =
    JP.decode Entry
        |> JP.required "id" JD.int
        |> JP.required "recordID" JD.int
        |> JP.required "page" JD.int
        |> JP.required "text" JD.string
        |> JP.optional "summary" (JD.nullable JD.string) Nothing


categoryDecoder : String -> Decoder Category
categoryDecoder s =
    let
        s_ =
            s |> String.toLower

        category =
            case s_ of
                "character" ->
                    Character

                "power" ->
                    Power

                "world" ->
                    World

                _ ->
                    Invalid
    in
        JD.succeed category
