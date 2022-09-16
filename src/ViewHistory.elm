module ViewHistory exposing (view)

import Context exposing (..)
import Dict
import Element.WithContext exposing (..)
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Screenshot
import ScreenshotsBy
import Shared exposing (..)
import SharedElement exposing (..)
import SharedTypes exposing (..)
import ViewFilter
import ViewScreenshot


view :
    { a | posix : Int, route : Route }
    -> ResponseAndCache
    -> Maybe Screenshot.Screenshot
    -> List (Element Context SharedTypes.Msg)
view model res maybeScreenshot1 =
    let
        qtyFilteredScreenshots : Int
        qtyFilteredScreenshots =
            List.length res.cache_screenshotsFiltered

        qtyUnfilteredScreenshots : Int
        qtyUnfilteredScreenshots =
            List.length res.response_screenshotsAll

        groupedByTime : Dict.Dict String (List Screenshot.Screenshot)
        groupedByTime =
            -- Should also this be cached?
            ScreenshotsBy.byTime res.cache_screenshotsFiltered
    in
    []
        ++ (case maybeScreenshot1 of
                Just screenshot ->
                    [ row
                        [ Font.size 80
                        , centerX
                        , spacing 10
                        , paddingXY 0 30
                        , moveUp 20
                        ]
                        [ screenshot.cat5
                            |> String.right 2
                            |> text
                            |> el
                                [ Font.color <| rgba 1 1 1 0.8
                                , Font.bold
                                ]
                        , screenshot.cat6
                            |> text
                            |> el
                                [ Font.color <| rgba 1 1 1 0.8
                                ]
                        ]
                    , el [ Border.rounded 10, clip, centerX ] <|
                        image []
                            { description = ""
                            , src = Screenshot.imageSrcToString <| Screenshot.toImageSrc screenshot
                            }
                    ]

                Nothing ->
                    [ ViewFilter.view model res.cache_screenshotsAllByCategory res.cache_screenshotsFilteredByCategory qtyUnfilteredScreenshots qtyFilteredScreenshots ]
           )
        ++ viewHelper model.posix maybeScreenshot1 groupedByTime


viewHelper :
    Int
    -> Maybe Screenshot.Screenshot
    -> Dict.Dict String (List Screenshot.Screenshot)
    -> List (Element Context Msg)
viewHelper posix maybeScreenshot1 dictCommits =
    dictCommits
        |> Dict.keys
        |> List.sort
        |> List.reverse
        |> List.map
            (\dateAndCommit ->
                column [ spacing 10, width fill ]
                    [ el [ paddingXY 10 0 ] <| humanDateAndCommit posix dateAndCommit
                    , let
                        filtereCommits : List Screenshot.Screenshot
                        filtereCommits =
                            dictCommits
                                |> Dict.get dateAndCommit
                                |> Maybe.withDefault []
                                |> Screenshot.filterOnlySameIgnoringTime maybeScreenshot1
                      in
                      row [ spacing 10, padding 10, scrollbarY, width fill ] <|
                        ([]
                            ++ List.map
                                (\screenshot ->
                                    ViewScreenshot.view maybeScreenshot1 screenshot
                                )
                                (filtereCommits
                                    |> List.take maxImagesPerRow
                                )
                            ++ (if List.length filtereCommits > maxImagesPerRow then
                                    [ column [ spacing 20, width <| px 100 ]
                                        [ paragraph []
                                            [ text <| "Showing " ++ String.fromInt maxImagesPerRow ++ " of " ++ String.fromInt (List.length filtereCommits) ++ " images"
                                            ]
                                        , paragraph []
                                            [ text <| "To see more, use the filter."
                                            ]
                                        ]
                                    ]

                                else
                                    []
                               )
                        )
                    ]
            )
