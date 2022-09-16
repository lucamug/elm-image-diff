module ViewLatest exposing (view)

import Context exposing (..)
import Dict
import Element.WithContext exposing (..)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import FeatherIcons
import Html.Attributes
import Screenshot
import ScreenshotsBy
import Shared
import SharedTypes exposing (..)
import ViewFilter
import ViewScreenshot


view :
    { a | posix : Int, route : Route }
    -> ResponseAndCache
    -> List (Element Context SharedTypes.Msg)
view model res =
    let
        qtyFilteredScreenshots : Int
        qtyFilteredScreenshots =
            List.length res.cache_screenshotsFiltered

        qtyUnfilteredScreenshots : Int
        qtyUnfilteredScreenshots =
            List.length res.response_screenshotsAll

        groupedById : Dict.Dict String (List Screenshot.Screenshot)
        groupedById =
            -- Should also this be cached?
            ScreenshotsBy.byId res.cache_screenshotsFiltered

        page : Int
        page =
            1

        listToDisplay :
            List
                { screenshotAsImageSrc : Screenshot.ImageSrc
                , screenshotId : String
                , x : Int
                , screenshot : Screenshot.Screenshot
                }
        listToDisplay =
            groupedById
                |> Dict.map
                    (\screenshotId screenshots ->
                        screenshots
                            |> List.map
                                (\screenshot ->
                                    { screenshotAsImageSrc = Screenshot.toImageSrc screenshot
                                    , screenshotId = screenshotId
                                    , x = screenshot.size.x
                                    , screenshot = screenshot
                                    }
                                )
                            -- Selecting the newest screenshot
                            -- Should be reversed?
                            |> List.sortBy (\item -> Screenshot.imageSrcToString item.screenshotAsImageSrc)
                            |> List.reverse
                            |> List.head
                            |> Maybe.withDefault
                                { screenshotAsImageSrc = Screenshot.toImageSrc Screenshot.empty
                                , screenshotId = ""
                                , x = 0
                                , screenshot = Screenshot.empty
                                }
                    )
                |> Dict.values
                |> List.sortBy .screenshotId
                |> List.drop ((page - 1) * Shared.itemsPerPage)
                |> List.take Shared.itemsPerPage
    in
    []
        ++ [ ViewFilter.view model res.cache_screenshotsAllByCategory res.cache_screenshotsFilteredByCategory qtyUnfilteredScreenshots qtyFilteredScreenshots ]
        ++ (if List.length listToDisplay == 0 then
                [ column
                    [ centerX
                    , paddingEach { top = 160, right = 0, bottom = 0, left = 0 }
                    , Font.size 24
                    , spacing 10
                    ]
                    [ text "No items to show!"
                    , el [ centerX ] <|
                        html
                            (FeatherIcons.alertCircle
                                |> FeatherIcons.withSize 60
                                |> FeatherIcons.withStrokeWidth 1
                                |> FeatherIcons.toHtml
                                    [ Html.Attributes.style "stroke"
                                        "#fff"
                                    ]
                            )
                    ]
                ]

            else
                []
                    ++ [ paragraph [ Font.size 20 ]
                            [ text "Showing "
                            , el [ Font.bold, paddingXY 5 0 ] <| text <| String.fromInt (((page - 1) * Shared.itemsPerPage) + 1) ++ " ~ " ++ String.fromInt (((page - 1) * Shared.itemsPerPage) + List.length listToDisplay)
                            , text " of "
                            , el [ Font.bold, paddingXY 5 0 ] <| text <| String.fromInt <| List.length <| Dict.keys groupedById
                            , text " unique images"
                            ]
                       ]
                    ++ [ wrappedRow [ spacing 20, centerX ] <|
                            (listToDisplay
                                |> List.map
                                    (\{ screenshotAsImageSrc, x, screenshot } ->
                                        column [ width <| px <| min x 450, alignTop ]
                                            [ image [ width fill, Border.rounded 5, clip ]
                                                { src = Screenshot.imageSrcToString screenshotAsImageSrc
                                                , description = ""
                                                }
                                            , ViewScreenshot.viewDetails [] screenshot
                                            ]
                                    )
                            )
                       ]
           )
