module ViewFilter exposing (view)

import Context exposing (..)
import Dict
import Element.WithContext exposing (..)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Filter
import Set
import Shared
import SharedElement exposing (..)
import SharedTypes exposing (..)


view :
    { a | posix : Int, route : Route }
    -> ScreenshotsByCategory
    -> ScreenshotsByCategory
    -> Int
    -> Int
    -> Element Context Msg
view model screenshotsAllByCategory screenshotsFilteredByCategory qtyUnfilteredScreenshots qtyFilteredScreenshots =
    column [ spacing 30, width fill, Background.color <| rgba 1 1 1 0.1, paddingXY 20 20, Border.rounded 10 ]
        ([]
            ++ [ paragraph [ Font.size 20 ] <|
                    []
                        ++ [ text <| "Selected " ]
                        ++ (if qtyUnfilteredScreenshots == qtyFilteredScreenshots then
                                [ el [ Font.bold, paddingXY 5 0 ] <| text <| "ALL" ]

                            else if qtyFilteredScreenshots == 0 then
                                [ el [ Font.bold, paddingXY 5 0 ] <| text <| "NONE" ]

                            else
                                [ el [ Font.bold, paddingXY 5 0 ] <| text <| String.fromInt qtyFilteredScreenshots ]
                           )
                        ++ [ text <| " of "
                           , el [ paddingXY 5 0 ] <| text <| String.fromInt qtyUnfilteredScreenshots
                           ]
                        ++ [ text " " ]
                        ++ (if qtyUnfilteredScreenshots == qtyFilteredScreenshots then
                                []

                            else
                                [ atomLinkInternal
                                    (buttonAttrs False ++ [ Font.size 16, paddingXY 10 5 ])
                                    { label = text "RESET to ALL"
                                    , route = changeFilterOfRoute Filter.empty model.route
                                    }
                                ]
                           )
               ]
            ++ [ let
                    categoriesList : List ( ScreenshotsByCategory -> Dict.Dict String (Set.Set String), String )
                    categoriesList =
                        [ ( .time, "time" )
                        , ( .size, "size" )
                        , ( .cat2, "cat2" )
                        , ( .cat3, "cat3" )
                        , ( .cat4, "cat4" )
                        , ( .cat5, "cat5" )
                        , ( .cat6, "cat6" )
                        , ( .cat7, "cat7" )
                        , ( .cat8, "cat8" )
                        , ( .cat9, "cat9" )
                        , ( .cat10, "cat10" )
                        ]
                 in
                 row [ spacing 10, scrollbarX, width fill ] <|
                    (categoriesList
                        |> List.map (f2 model screenshotsAllByCategory screenshotsFilteredByCategory)
                        |> List.concat
                        |> (\list -> List.take (List.length list - 1) list)
                    )
               ]
        )


f2 :
    { b | posix : Int, route : Route }
    -> a
    -> a
    -> ( a -> Dict.Dict String (Set.Set String), String )
    -> List (Element Context Msg)
f2 model screenshotsAllByCategory screenshotsFilteredByCategory ( category, categoryLabel ) =
    let
        filter : Filter.Filter
        filter =
            routeToFilter model.route
    in
    [ column
        [ spacing 0
        , alignTop
        ]
        ([]
            ++ (let
                    allFiltersForThisCategory : Filter.Filter
                    allFiltersForThisCategory =
                        Filter.allFiltersForThisCategory category categoryLabel screenshotsAllByCategory
                in
                [ row [ spacing 10 ]
                    [ atomLinkInternal
                        (buttonAttrs False ++ [ Font.size 14, paddingXY 10 5 ])
                        { label = text "ALL"
                        , route = changeFilterOfRoute (Filter.diff filter allFiltersForThisCategory) model.route
                        }
                    , atomLinkInternal
                        (buttonAttrs False ++ [ Font.size 14, paddingXY 10 5 ])
                        { label = text "NONE"
                        , route = changeFilterOfRoute (Filter.union filter allFiltersForThisCategory) model.route
                        }
                    ]
                ]
               )
            ++ [ column
                    [ height (fill |> maximum 273)
                    , scrollbars
                    , width fill
                    ]
                 <|
                    viewToggler
                        model
                        category
                        categoryLabel
                        screenshotsAllByCategory
                        screenshotsFilteredByCategory
               ]
        )
    , el
        [ Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }
        , Border.color <| rgba 1 1 1 0.2
        , height fill
        ]
        none
    ]


viewToggler :
    { a | posix : Int, route : Route }
    -> (b -> Dict.Dict String (Set.Set String))
    -> String
    -> b
    -> b
    -> List (Element Context Msg)
viewToggler model category categoryKey screenshotsAllByCategory screenshotsFilteredByCategory =
    let
        filter : Filter.Filter
        filter =
            routeToFilter model.route
    in
    List.map
        (\( categoryValue, _ ) ->
            let
                newFilter : Filter.Filter
                newFilter =
                    Filter.newFilter
                        { categoryKey = categoryKey
                        , categoryValue = categoryValue
                        }
                        filter

                member : Bool
                member =
                    Filter.memberCategory
                        { categoryKey = categoryKey
                        , categoryValue = categoryValue
                        }
                        filter
            in
            atomLinkInternal
                [ paddingXY 8 8
                , mouseOver [ Background.color <| rgba 0 0 0 0.2 ]
                , Background.color <|
                    if member then
                        rgba 0 0 0 0.1

                    else
                        rgba 0 0 0 0
                , Border.rounded 5
                , width fill
                ]
                { label =
                    row
                        [ spacing 10, width fill ]
                        [ el
                            [ Border.rounded 20
                            , Background.color <| rgba 0 0 0 0.3
                            , width <| px 30
                            , height <| px 20
                            , inFront <|
                                el
                                    [ Border.rounded 20
                                    , transition "transform 0.1s"
                                    , Background.color <|
                                        if member then
                                            rgba 1 1 1 0.3

                                        else
                                            rgba 0.4 0.7 1 1
                                    , moveRight <|
                                        if member then
                                            2

                                        else
                                            12
                                    , width <| px 16
                                    , height <| px 16
                                    , moveDown 2
                                    ]
                                <|
                                    none
                            ]
                          <|
                            none
                        , humanDateAndCommitShort categoryValue

                        --
                        , screenshotsFilteredByCategory
                            |> category
                            |> Dict.get categoryValue
                            |> Maybe.map Set.size
                            |> Maybe.withDefault 0
                            |> Shared.addThousandSeparator
                            |> text
                            |> el
                                [ Font.color <| rgba 1 1 1 0.5
                                , Font.size 14
                                , alignRight
                                ]
                        ]
                , route = changeFilterOfRoute newFilter model.route
                }
        )
        (screenshotsAllByCategory
            |> category
            |> Dict.toList
            -- |> Debug.log "xxx"
            |> (if categoryKey == "time" then
                    List.reverse

                else
                    identity
               )
         -- |> (\( string, _ ) ->
         --         if string == "time" then
         --             List.reverse
         --
         --         else
         --             identity
         --    )
        )
