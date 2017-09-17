module Main exposing (main)

import Html exposing (Html)
import Http
import Element exposing (..)
import Element.Attributes exposing (..)
import Element.Input as Input
import Style exposing (..)
import Style.Color as Color
import Style.Font as Font
import Style.Border as Border
import Color exposing (..)
import Simple.Fuzzy as Fuzzy


--

import Savage.Data as Data


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { allRecords : List Data.Record
    , displayRecords : List Data.Record
    , currentRecord : Maybe Data.Record
    , filters : Filters
    , error : String
    , entries : List Data.Entry
    }


type alias Filters =
    { limitPage : Int
    , maxPage : Int
    , category : Data.Category
    , text : Maybe String
    }


model : Model
model =
    { allRecords = []
    , displayRecords = []
    , currentRecord = Nothing
    , filters =
        { limitPage = 1
        , maxPage = 1
        , category = Data.Character
        , text = Nothing
        }
    , entries =
        [ { id = 1
          , recordID = 1
          , chapter = 1
          , text = "Rayne awakes in the slave camp"
          , summary = Nothing
          }
        ]
    , error = "No Errors"
    }


init : ( Model, Cmd Msg )
init =
    model ! [ fetchDataCmd ]



-- UPDATE


type Msg
    = NoOp
    | FetchDataCompleted (Result Http.Error Data.JsonRoot)
    | UpdatePage String
    | SelectCategory Data.Category
    | UpdateSearch String
    | SelectRecord Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ filters } as model) =
    case msg of
        NoOp ->
            model ! []

        FetchDataCompleted result ->
            case result of
                Ok { records, entries, maxPage } ->
                    let
                        filters_ =
                            { filters | maxPage = maxPage }
                    in
                        { model
                            | allRecords = records
                            , displayRecords = filterRecords filters records
                            , entries = entries
                            , filters = filters_
                        }
                            ! []

                _ ->
                    model ! []

        UpdatePage page ->
            let
                page_ =
                    page
                        |> String.toInt
                        |> Result.withDefault 0

                filters_ =
                    { filters | limitPage = page_ }
            in
                { model
                    | filters = filters_
                    , displayRecords = filterRecords filters_ model.allRecords
                }
                    ! []

        SelectCategory category ->
            model ! []

        UpdateSearch search ->
            let
                search_ =
                    case String.isEmpty search of
                        False ->
                            Just search

                        True ->
                            Nothing

                filters_ =
                    { filters | text = search_ }
            in
                { model
                    | filters = filters_
                    , displayRecords = filterRecords filters_ model.allRecords
                }
                    ! []

        SelectRecord recordID ->
            model ! []


fetchDataCmd : Cmd Msg
fetchDataCmd =
    Http.send FetchDataCompleted (Http.get "data.json" Data.dataDecoder)


filterRecords : Filters -> List Data.Record -> List Data.Record
filterRecords filters records =
    records
        |> List.filter (\r -> r.category == filters.category)
        |> List.filter (\r -> r.firstPage <= filters.limitPage)
        |> Fuzzy.filter .title (Maybe.withDefault "" filters.text)



--|> List.sortWith Data.recordOrdering
-- VIEW


type Styles
    = None
    | Page
    | SideContainer
    | SideItems
    | SideItem
    | ContentContainer
    | ContentSummary
    | ContentItems
    | ContentItem
    | ChapterNumber
    | Header
    | Category
    | Selected



--http://www.colourlovers.com/palette/85232/Starry_Night


stylesheet : StyleSheet Styles variation
stylesheet =
    Style.styleSheet
        [ style Page
            [ Color.background (rgb 0 0 0)
            , Color.text white
            , Font.typeface [ Font.font "Open Sans", Font.font "sans-serif" ]
            , Font.size 18
            ]
        , style SideContainer
            [ Color.background (rgb 120 144 168)
            , Color.text white
            ]
        , style ContentContainer
            [ Color.background (rgb 48 72 120)
            , Color.text white
            ]
        , style ContentSummary
            []
        , style ContentItems
            []
        , style ContentItem
            []
        , style ChapterNumber
            [ Border.right 1
            , Color.border white
            , Font.alignRight
            ]
        , style Header
            [ Color.background (rgb 24 24 72)
            , Font.typeface [ Font.font "Shadows Into Light", Font.font "cursive" ]
            , Font.size 30
            , Font.lineHeight 1
            ]
        , style Selected
            [ Color.background (rgb 240 168 24)
            ]
        ]


view : Model -> Html Msg
view model =
    viewport
        stylesheet
        (row Page
            [ width fill
            , padding 20
            , spacing 20
            , spread
            ]
            [ side model
            , content model
            ]
        )


side model =
    column SideContainer
        [ width (px 400)
        , spacing 10
        ]
        [ el Header
            [ paddingXY 20 10 ]
            (text "the Savage Codex")
          --, hairline Selected
        , pageFilter model.filters
          --, hairline Selected
        , categoryFilter model.filters
          --, hairline Selected
        , searchFilter model.filters
          --, hairline Selected
        , itemList model.displayRecords
        ]


pageFilter filters =
    row None
        [ spread
        , paddingXY 10 0
        ]
        [ Input.text None
            [ padding 10 ]
            { onChange = UpdatePage
            , value = (toString filters.limitPage)
            , label = Input.hiddenLabel "page slider"
            , options = []
            }
        , Input.text None
            [ width (px 100)
            , padding 10
            ]
            { onChange = UpdatePage
            , value = (toString filters.limitPage)
            , label = Input.hiddenLabel "page input"
            , options = []
            }
        ]


categoryFilter filters =
    row None
        [ alignLeft
        , paddingXY 10 0
        , spacing 10
        ]
        [ el Selected [ padding 10 ] (text "Characters")
        , el Selected [ padding 10 ] (text "Powers")
        , el Selected [ padding 10 ] (text "Factions")
        , el Selected [ padding 10 ] (text "World")
        ]


searchFilter filters =
    row None
        [ spread
        , paddingXY 10 0
        ]
        [ Input.search None
            [ padding 10
            , width fill
            ]
            { onChange = UpdateSearch
            , value = filters.text |> Maybe.withDefault ""
            , label =
                Input.placeholder
                    { text = "Search"
                    , label = Input.hiddenLabel "search"
                    }
            , options = []
            }
        ]


itemList records =
    let
        record r =
            el None
                [ paddingXY 20 10 ]
                (text r.title)
    in
        column None
            [ spread ]
            (List.map record records)


content model =
    column ContentContainer
        [ width fill
        , minWidth (px 400)
        ]
        [ el Header
            [ paddingXY 20 10 ]
            (text "Nothing here yet")
        , el ContentSummary
            [ paddingXY 20 10 ]
            (text "No Summary")
        , contentItems
        ]


contentItems =
    column ContentItems
        []
        [ contentItem "one"
        , contentItem "two"
        , contentItem "three"
        , contentItem "four"
        ]


contentItem i =
    row ContentItem
        [ paddingXY 20 5
        , spacing 10
        , alignLeft
        ]
        [ el ChapterNumber
            [ width (px 60)
            , paddingRight 10
            ]
            (text "999")
        , el None [] (text i)
        ]
