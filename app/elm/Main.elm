module Main exposing (main)

import Html exposing (Html, div, text)


main =
    Html.program
        { init = ( init, Cmd.none )
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { skeleton : String }


init : Model
init =
    Model "Brunch + Elm"



-- UDPATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text model.skeleton ]
