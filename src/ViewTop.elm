module ViewTop exposing (view)

import Context exposing (..)
import Element.WithContext exposing (..)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Shared exposing (..)
import SharedElement exposing (..)
import SharedTypes exposing (..)


buttonAttrs : List (Attribute Context Msg)
buttonAttrs =
    [ mouseOver [ Background.color <| rgba 0 0 0 0.1 ]
    , paddingXY 20 10
    , width fill
    , Border.rounded 5
    , SharedElement.transition "background-color 0.2s"
    ]


view : a -> ResponseAndCache -> ElementC Msg
view _ res =
    column [ spacing 115, paddingEach { top = 115, right = 0, bottom = 0, left = 0 }, centerX ]
        [ column
            [ centerX
            , spacing 10
            ]
            [ el [ Font.size 70, centerX ] <|
                text <|
                    addThousandSeparator <|
                        List.length res.response_screenshotsAll
            , paragraph
                [ Font.size 16
                , Font.center
                , style "letter-spacing" "8px"
                ]
                [ text "SCREENSHOTS" ]
            ]
        , column [ spacing 40 ]
            [ el [ centerX ] <| text "Examples"
            , row []
                [ column [ spacing 0 ]
                    [ atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Desktop views" ]
                        , url = "/latest?f=cat3__dark~cat4__de-de~cat4__es-es~cat4__fr-fr~cat4__ja-jp~cat4__uk-ua~cat4__zh-cn~cat4__zh-tw~cat7__error-api~cat7__error-validation~size__320x1000~size__320x6000~size__840x6000"
                        }
                    , atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Mobile views" ]
                        , url = "/latest?f=cat3__dark~cat4__de-de~cat4__es-es~cat4__fr-fr~cat4__ja-jp~cat4__uk-ua~cat4__zh-cn~cat4__zh-tw~cat7__error-api~cat7__error-validation~size__320x6000~size__840x1200~size__840x6000"
                        }
                    , atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Dark mode" ]
                        , url = "/latest?f=cat3__light~cat4__de-de~cat4__es-es~cat4__fr-fr~cat4__ja-jp~cat4__uk-ua~cat4__zh-cn~cat4__zh-tw~cat7__error-api~cat7__error-validation~size__320x1000~size__320x6000~size__840x6000"
                        }
                    , atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Reset password flow" ]
                        , url = "/latest?f=cat3__dark~cat4__de-de~cat4__es-es~cat4__fr-fr~cat4__ja-jp~cat4__uk-ua~cat4__zh-cn~cat4__zh-tw~cat7__error-api~cat7__error-validation~cat8__agreement~cat8__email-verification~cat8__merge~cat8__none~cat8__progressive~cat8__registration~cat8__sign-in~cat8__test~size__320x1000~size__320x6000~size__840x6000"
                        }
                    , atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Sign-in, all languages" ]
                        , url = "/latest?f=cat3__dark~cat5__003~cat5__005~cat5__006~cat5__007~cat5__011~cat5__012~cat5__014~cat5__016~cat5__030~cat5__031~cat5__040~cat5__045~cat5__051~cat5__054~cat5__055~cat5__057~cat5__058~cat5__071~cat5__072~cat5__073~cat5__080~cat5__099~cat7__error-api~cat7__error-validation~size__320x6000~size__840x1200~size__840x6000"
                        }
                    ]
                , column [ spacing 0 ]
                    [ atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Validation errors" ]
                        , url = "/latest?f=cat3__dark~cat4__de-de~cat4__es-es~cat4__fr-fr~cat4__ja-jp~cat4__uk-ua~cat4__zh-cn~cat4__zh-tw~cat7__error-api~cat7__normal~size__320x6000~size__840x1200~size__840x6000"
                        }
                    , atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Api errors" ]
                        , url = "/latest?f=cat3__dark~cat4__de-de~cat4__es-es~cat4__fr-fr~cat4__ja-jp~cat4__uk-ua~cat4__zh-cn~cat4__zh-tw~cat7__error-validation~cat7__normal~size__320x6000~size__840x1200~size__840x6000"
                        }
                    , atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Error \"500\", all languages" ]
                        , url = "/latest?f=cat10__jid-error~cat10__none~cat10__r400-expired-token~cat10__r400-password-reuse-password~cat10__r400-wrong-challengecid~cat10__r403-device-registration-cancelled~cat10__r403-invalid-client~cat10__r403-login-rejected~cat10__r404-user-not-found~cat10__r408-timeout~cat10__r409-ambiguous-login~cat10__r429-device-registration-attempts-exceed~cat10__submitted-empty~cat3__dark~size__320x6000~size__840x1200~size__840x6000"
                        }
                    , atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "Images difference" ]
                        , url = "/diff/2021-06-15T12.06.00Z_74f2feed_320x1000_ua_light_uk-ua_001_a_error-api_sign-in_none_r500-external-api-error_5091/2021-06-15T05.47.02Z_98865936_320x1000_ua_light_uk-ua_001_a_error-api_sign-in_none_r500-external-api-error_0?f=cat10__jid-error~cat10__none~cat10__r400-expired-token~cat10__r400-password-reuse-password~cat10__r400-wrong-challengecid~cat10__r403-device-registration-cancelled~cat10__r403-invalid-client~cat10__r403-login-rejected~cat10__r404-user-not-found~cat10__r408-timeout~cat10__r409-ambiguous-login~cat10__r429-device-registration-attempts-exceed~cat10__submitted-empty~cat3__dark~size__320x6000~size__840x1200~size__840x6000"
                        }
                    , atomLinkInternalWithUrlAsString buttonAttrs
                        { label = row [ spacing 15 ] [ el [ Font.size 12 ] <| text "▶", text "History" ]
                        , url = "/history"
                        }
                    ]
                ]
            ]
        ]
