module ViewScreenshot exposing (view, viewDetails)

import Context exposing (..)
import Element.WithContext exposing (..)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import FeatherIcons
import Html
import Html.Attributes
import Screenshot
import Shared exposing (..)
import SharedElement exposing (..)
import SharedTypes exposing (..)


view : Maybe Screenshot.Screenshot -> Screenshot.Screenshot -> Element Context Msg
view maybeSelected screenshot =
    -- The preview has a fix size of 210x300
    column [ spacing 10, width <| px 210 ]
        [ el
            [ height <| px 300
            , Border.rounded 10
            , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 10, color = rgba 0 0 0 0.2 }

            -- , inFront <|
            --     el
            --         [ Background.gradient
            --             { angle = 0
            --             , steps =
            --                 [ rgba 1 1 1 0.2
            --                 , rgba 0 0 0 0
            --                 , rgba 0 0 0 0
            --                 , rgba 0 0 0 0
            --                 ]
            --             }
            --         , width fill
            --         , height fill
            --         ]
            --         none
            , row
                [ alignBottom
                , alignRight
                , moveLeft 20
                , moveUp 20
                , Font.size 40
                , spacing 2
                , style "pointer-events" "none"
                ]
                [ screenshot.cat5
                    |> String.right 2
                    |> text
                    |> el
                        [ Font.color <| rgb 0.5 0.5 0.5
                        , Font.bold
                        ]
                , screenshot.cat6
                    |> text
                    |> el
                        [ Font.color <| rgb 0.5 0.5 0.5
                        ]
                ]
                |> inFront
            , clip
            ]
            (image
                ([]
                    ++ [ alignTop
                       , width fill
                       ]
                    ++ (if screenshot.differentPixels == 0 then
                            [ inFront <|
                                el
                                    [ alignRight
                                    , Background.color <| rgba 0.5 0.5 0.5 0.7
                                    , Border.rounded 5
                                    , paddingXY 4 3
                                    , Font.size 13
                                    , Font.bold
                                    , moveDown 5
                                    , moveLeft 5
                                    , Font.color <| rgb 1 1 1
                                    ]
                                <|
                                    text "NEW"
                            ]

                        else
                            []
                       )
                    ++ [ inFront <|
                            el
                                [ Background.gradient
                                    { angle = 0
                                    , steps =
                                        [ rgba 0 0 0 0
                                        , rgba 0 0 0 0
                                        , rgba 0 0 0 0
                                        , rgba 0 0 0 0.2
                                        ]
                                    }
                                , alpha <|
                                    if maybeSelected == Just screenshot then
                                        1

                                    else
                                        0
                                , mouseOver [ alpha 1 ]
                                , transition "all 0.2s"
                                , width fill
                                , height fill
                                , inFront <| viewCheckbox maybeSelected screenshot
                                ]
                                none
                       ]
                )
                { description = ""
                , src = Screenshot.imageSrcToString <| Screenshot.toImageSrc screenshot
                }
            )
        , viewDetails [] screenshot
        ]


viewDetails : List (Attribute context msg) -> Screenshot.Screenshot -> Element context msg
viewDetails attrs screenshot =
    screenshot
        |> Screenshot.toScreenshotId
        |> Screenshot.screenshotIdToString
        |> String.replace "_" "\n* "
        |> (++) "* "
        |> Html.text
        |> (\htmlElement -> Html.pre [ Html.Attributes.style "font-size" "14px" ] [ htmlElement ])
        |> html
        |> el ([ scrollbars, width fill ] ++ attrs)


viewCheckbox :
    Maybe Screenshot.Screenshot
    -> Screenshot.Screenshot
    -> Element Context Msg
viewCheckbox maybeSelected screenshot =
    let
        isSelected : Bool
        isSelected =
            maybeSelected == Just screenshot
    in
    withContext <|
        \c ->
            atomLinkInternal
                [ Background.color <|
                    if isSelected then
                        rgba 0 0.6 1 1

                    else
                        rgba 1 1 1 1
                , width <| px 30
                , height <| px 30
                , moveRight 10
                , moveDown 10
                , Border.rounded 60
                , inFront <|
                    el [ centerX, centerY ] <|
                        html <|
                            (FeatherIcons.check
                                |> FeatherIcons.withSize 24
                                |> FeatherIcons.withStrokeWidth 3
                                |> FeatherIcons.toHtml
                                    [ Html.Attributes.style "stroke"
                                        (if isSelected then
                                            "#fff"

                                         else
                                            "#999"
                                        )
                                    ]
                            )
                ]
                { label = none
                , route =
                    if isSelected then
                        Top c.filter

                    else
                        case maybeSelected of
                            Just selected ->
                                Selected2 c.filter selected screenshot

                            Nothing ->
                                Selected1 c.filter screenshot
                }
