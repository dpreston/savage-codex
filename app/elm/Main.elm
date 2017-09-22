module Main exposing (main)

import Html exposing (Html)
import Http
import Element exposing (..)
import Element.Input as Input
import Element.Attributes exposing (..)
import Element.Events exposing (onClick)
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

                Err (Http.BadPayload error _) ->
                    let
                        _ =
                            Debug.log error "error"
                    in
                        { model | error = error } ! []

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
            let
                filters_ =
                    { filters | category = category }
            in
                { model
                    | filters = filters_
                    , displayRecords = filterRecords filters_ model.allRecords
                }
                    ! []

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

        SelectRecord id ->
            let
                record =
                    model.allRecords
                        |> List.filter (\r -> r.id == id)
                        |> List.head
            in
                { model | currentRecord = record } ! []


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
    | Header
    | SideContainer
    | CategorySelect
    | SideList
    | SideItem
    | ContentContainer
    | ContentSummary
    | ContentList
    | ContentItem
    | ChapterNumber


type Variations
    = Selected



--http://www.colourlovers.com/palette/85232/Starry_Night


stylesheet : StyleSheet Styles Variations
stylesheet =
    Style.styleSheet
        [ style None []
        , style Page
            [ Color.background (rgb 0 0 0)
            , Color.text white
            , Font.typeface [ Font.font "Open Sans", Font.font "sans-serif" ]
            , Font.size 18
            ]
        , style Header
            [ Color.background (rgb 24 24 72)
            , Font.typeface [ Font.font "Shadows Into Light", Font.font "cursive" ]
            , Font.size 30
            , Font.lineHeight 1
            ]
        , style SideContainer
            [ Color.background (rgb 120 144 168)
            , Color.text white
            ]
        , style CategorySelect
            [ Border.none
            , Color.background (rgb 255 255 255)
            , variation Selected
                [ Color.background (rgb 240 168 24)
                ]
            ]
        , style SideList
            []
        , style SideItem
            [ variation Selected
                [ Color.background (rgb 240 168 24) ]
            ]
        , style ContentContainer
            [ Color.background (rgb 48 72 120)
            , Color.text white
            ]
        , style ContentSummary
            []
        , style ContentList
            []
        , style ContentItem
            []
        , style ChapterNumber
            [ Border.right 1
            , Color.border white
            , Font.alignRight
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
        [ el Header [ paddingXY 20 10 ] (text "the Savage Codex")
        , pageFilters model.filters
        , categoryFilter model.filters
        , searchFilter model.filters
        , itemList model.displayRecords model.currentRecord
        ]


pageFilters filters =
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
          --, spacing 10
        ]
        [ button CategorySelect
            [ padding 10
            , vary Selected (filters.category == Data.Character)
            , onClick (SelectCategory Data.Character)
            ]
            (text "Characters")
        , button CategorySelect
            [ padding 10
            , vary Selected (filters.category == Data.Power)
            , onClick (SelectCategory Data.Power)
            ]
            (text "Powers")
        , button CategorySelect
            [ padding 10
            , vary Selected (filters.category == Data.World)
            , onClick (SelectCategory Data.World)
            ]
            (text "World")
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


itemList records current =
    let
        currentID =
            case current of
                Just r ->
                    r.id

                Nothing ->
                    0

        record r =
            el SideItem
                [ paddingXY 20 10
                , vary Selected (r.id == currentID)
                , onClick (SelectRecord r.id)
                ]
                (text r.title)
    in
        column SideList
            [ spread ]
            (List.map record records)


content model =
    let
        maybeToBool attr =
            case attr of
                Just _ ->
                    True

                Nothing ->
                    False

        record_ =
            case model.currentRecord of
                Just r ->
                    r

                Nothing ->
                    Data.emptyRecord

        displayEntries record =
            record.entries
                |> List.filter (\e -> e.chapter <= model.filters.limitPage)
                |> List.sortBy .chapter

        displaySummary entries =
            entries
                |> List.filter (\e -> maybeToBool e.summary)
                |> List.reverse
                |> List.head
                |> Maybe.withDefault Data.emptyEntry
                |> .summary
                |> Maybe.withDefault "huh?"

        contentList entries =
            column ContentList
                []
                (List.map contentItem entries)

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
                    (text (toString i.chapter))
                , el None [] (text i.text)
                ]
    in
        column ContentContainer
            [ width fill
            , minWidth (px 400)
            ]
            [ el Header
                [ paddingXY 20 10 ]
                (text record_.title)
            , el ContentSummary
                [ paddingXY 20 10 ]
                (text (displaySummary (displayEntries record_)))
            , contentList (displayEntries record_)
            ]
