module ViewDiff exposing (view)

import Context exposing (..)
import Element.WithContext exposing (..)
import Element.WithContext.Border as Border
import Element.WithContext.Input as Input
import FeatherIcons
import Html
import Html.Attributes
import Html.Events
import Json.Decode
import Screenshot
import SharedElement exposing (..)
import SharedTypes exposing (..)
import ViewScreenshot


view :
    Model
    -> Screenshot.Screenshot
    -> Screenshot.Screenshot
    -> List (Element Context Msg)
view model screenshot1 screenshot2 =
    let
        widthPreview : Int
        widthPreview =
            min 500 screenshot1.size.x
    in
    []
        ++ [ row [ spacing 30 ]
                [ withContext <|
                    \c ->
                        atomLinkInternal []
                            { label =
                                el [ moveRight 6 ] <|
                                    html
                                        (FeatherIcons.chevronLeft
                                            |> FeatherIcons.withSize 40
                                            |> FeatherIcons.withStrokeWidth 1
                                            |> FeatherIcons.toHtml
                                                [ Html.Attributes.style "stroke"
                                                    "#fff"
                                                ]
                                        )
                            , route = Top c.filter
                            }
                , Input.button
                    (buttonAttrs (model.diffBlinkingSpeed == 0))
                    { label = text "NO BLINK"
                    , onPress = Just <| ChangeDiffBlinkingSpeed 0
                    }
                , Input.button
                    (buttonAttrs (model.diffBlinkingSpeed == 2000))
                    { label = text "BLINK"
                    , onPress = Just <| ChangeDiffBlinkingSpeed 2000
                    }
                , Input.button
                    (buttonAttrs (model.diffBlinkingSpeed == 500))
                    { label = text "BLINK FAST"
                    , onPress = Just <| ChangeDiffBlinkingSpeed 500
                    }
                ]
           ]
        ++ (let
                attrs : List (Attribute context msg)
                attrs =
                    [ style "animation-name" "fade"
                    , style "animation-iteration-count" "infinite"
                    , style "animation-duration" (String.fromInt model.diffBlinkingSpeed ++ "ms")
                    , style "animation-timing-function" "linear"
                    ]
            in
            [ column
                [ scrollbars
                , width fill
                , inFront <| html <| Html.node "style" [] [ Html.text """
                                        @keyframes fade {
                                            0% {opacity: 0}
                                            10% {opacity: 1}
                                            50% {opacity: 1}
                                            60% {opacity: 0}
                                            100% {opacity: 0}
                                        }""" ]
                ]
                [ row
                    [ spacing 20
                    , padding 10
                    , width fill
                    , scrollbarX
                    , height fill
                    ]
                    [ column [ alignTop, spacing 20 ]
                        [ el [ width <| px widthPreview ] <| el [ centerX ] <| innerLabel "BEFORE"
                        , image
                            [ htmlAttribute <| Html.Events.on "mousemove" (Json.Decode.map (MouseMove "before") decoder)
                            , htmlAttribute <| Html.Events.onMouseLeave MouseLeave
                            , style "cursor" "col-resize"
                            , width <| px widthPreview
                            , Border.rounded 10
                            , clip
                            , inFront <|
                                el
                                    [ clip
                                    , width (fill |> maximum model.offsetXbefore)
                                    , Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }
                                    , Border.color <| rgba 0.5 0.5 0.5 0.5
                                    ]
                                <|
                                    viewCanvas
                                        attrs
                                        { id = "canvas2", screenSize = screenshot2.size, widthPreview = widthPreview }
                            ]
                            { src = Screenshot.imageSrcToString <| Screenshot.toImageSrc screenshot1
                            , description = ""
                            }
                        , ViewScreenshot.viewDetails [] screenshot1
                        ]
                    , column [ alignTop, spacing 20 ]
                        [ el [ width <| px widthPreview ] <| el [ centerX ] <| row [ spacing 10 ] [ innerLabel "- DELETED", innerLabel "SAME", innerLabel "+ ADDED" ]
                        , viewCanvas [] { id = "diff", screenSize = screenshot1.size, widthPreview = widthPreview }
                        ]
                    , column [ alignTop, spacing 20 ]
                        [ el [ width <| px widthPreview ] <| el [ centerX ] <| innerLabel "AFTER"
                        , image
                            [ htmlAttribute <| Html.Events.on "mousemove" (Json.Decode.map (MouseMove "after") decoder)
                            , htmlAttribute <| Html.Events.onMouseLeave MouseLeave
                            , style "cursor" "col-resize"
                            , width <| px widthPreview
                            , Border.rounded 10
                            , clip
                            , inFront <|
                                el
                                    [ clip
                                    , width (fill |> maximum model.offsetXafter)
                                    , Border.widthEach { bottom = 0, left = 0, right = 1, top = 0 }
                                    , Border.color <| rgba 0.5 0.5 0.5 0.5
                                    ]
                                <|
                                    viewCanvas
                                        attrs
                                        { id = "canvas1", screenSize = screenshot1.size, widthPreview = widthPreview }
                            ]
                            { src = Screenshot.imageSrcToString <| Screenshot.toImageSrc screenshot2
                            , description = ""
                            }
                        , ViewScreenshot.viewDetails [] screenshot2
                        ]
                    ]
                ]
            ]
           )


decoder : Json.Decode.Decoder MouseMoveData
decoder =
    Json.Decode.map4 MouseMoveData
        (Json.Decode.at [ "offsetX" ] Json.Decode.int)
        (Json.Decode.at [ "offsetY" ] Json.Decode.int)
        (Json.Decode.at [ "target", "offsetHeight" ] Json.Decode.float)
        (Json.Decode.at [ "target", "offsetWidth" ] Json.Decode.float)
