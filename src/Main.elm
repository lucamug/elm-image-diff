port module Main exposing (Flags, main)

import Browser
import Browser.Dom
import Codec
import Context exposing (..)
import Crypto.Hash
import Element.WithContext exposing (..)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Events as Events
import Element.WithContext.Font as Font
import Element.WithContext.Input as Input
import FeatherIcons
import Filter
import FilterApplier
import Html
import Html.Attributes
import Http
import Json.Decode
import Maybe.Extra
import Screenshot
import ScreenshotsBy
import SharedElement
import SharedTypes exposing (..)
import Task
import Url
import Url.Parser exposing ((</>), (<?>))
import Url.Parser.Query
import ViewDiff
import ViewHeader
import ViewHistory
import ViewLatest
import ViewTop


styleWebsite : StyleWebsite
styleWebsite =
    Unbranded


type StyleWebsite
    = Unbranded



--|     LocalStorage


type alias LocalStorage =
    { mode : Mode
    , language : Language
    , hashedFp : Maybe String
    , infoClosed : Bool
    , diffBlinkingSpeed : Int
    }



--|     Language
--|     Flags


type alias Flags =
    { localStorage : String
    , locationHref : String
    , posix : Int
    , fp : String
    }



--  ██████  ██████  ███    ██ ███████ ██  ██████  ██    ██ ██████   █████  ████████ ██  ██████  ███    ██
-- ██      ██    ██ ████   ██ ██      ██ ██       ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ████   ██
-- ██      ██    ██ ██ ██  ██ █████   ██ ██   ███ ██    ██ ██████  ███████    ██    ██ ██    ██ ██ ██  ██
-- ██      ██    ██ ██  ██ ██ ██      ██ ██    ██ ██    ██ ██   ██ ██   ██    ██    ██ ██    ██ ██  ██ ██
--  ██████  ██████  ██   ████ ██      ██  ██████   ██████  ██   ██ ██   ██    ██    ██  ██████  ██   ████
--| CONFIGURATION


configuration : Configuration
configuration =
    -- conf contins things that don't change during the life of the app
    -- { nameWebsite = "Image 🦉 Diff"
    { nameWebsite = ""
    , mediaLocation = "https://training-assets.surge.sh/media/"
    , gridMinimum = 165
    , gridMaximum = 340
    , maxWidth = 1000
    , debugging = False
    , paletteLight = paletteLight
    , paletteDark = paletteDark
    }



-- ██ ███    ██ ██ ████████
-- ██ ████   ██ ██    ██
-- ██ ██ ██  ██ ██    ██
-- ██ ██  ██ ██ ██    ██
-- ██ ██   ████ ██    ██
--| INIT


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        model : Model
        model =
            { mode = Light
            , language = EN_US
            , route = locationHrefToRoute flags.locationHref
            , httpRequest = Fetching
            , warnings = []
            , hideOverview = False
            , pwd = ""
            , infoClosed = False
            , posix = flags.posix
            , diffBlinkingSpeed = 0
            , hashedFp = masha flags.fp
            , maybeHashedLocationHref =
                flags
                    |> .locationHref
                    |> Url.fromString
                    |> Maybe.map (\url -> { url | path = "", query = Nothing, fragment = Nothing })
                    |> Maybe.map Url.toString
                    |> Maybe.map masha
            , maybeHashedFpInLocalStorage = Nothing
            , offsetXbefore = 10000
            , offsetXafter = 10000
            }
                |> (\m ->
                        --
                        -- `flags` is a string stored in the local storage containing
                        -- the model used to initialize our app on start.
                        -- https://guide.elm-lang.org/interop/flags.html
                        --
                        -- The string required to be parsed to check if it is well
                        -- formed.
                        --
                        case stringToLocalStorage flags.localStorage of
                            Ok localStorage ->
                                localStorageToModel localStorage m

                            Err _ ->
                                --
                                -- If no data in the local storage or the data is broken,
                                -- we initialize the model from scratch.
                                --
                                m
                   )
    in
    ( model, commandToRequestScreenshots )
        |> andThen updateErrors DoNothing



-- ██████   ██████  ██    ██ ████████ ██ ███    ██  ██████
-- ██   ██ ██    ██ ██    ██    ██    ██ ████   ██ ██
-- ██████  ██    ██ ██    ██    ██    ██ ██ ██  ██ ██   ███
-- ██   ██ ██    ██ ██    ██    ██    ██ ██  ██ ██ ██    ██
-- ██   ██  ██████   ██████     ██    ██ ██   ████  ██████
--| ROUTING


urlParser : Url.Parser.Parser (Route -> a) a
urlParser =
    Url.Parser.oneOf
        [ Url.Parser.map (\maybeString -> Top (maybeStringToFilter maybeString)) (Url.Parser.top <?> Url.Parser.Query.string "f")
        , Url.Parser.map (\maybeString -> Latest (maybeStringToFilter maybeString)) (Url.Parser.s "latest" <?> Url.Parser.Query.string "f")
        , Url.Parser.map (\maybeString -> History (maybeStringToFilter maybeString)) (Url.Parser.s "history" <?> Url.Parser.Query.string "f")
        , Url.Parser.map (\string maybeString -> DetailsImage (maybeStringToFilter maybeString) (Image string)) (Url.Parser.s "image" </> Url.Parser.string <?> Url.Parser.Query.string "f")
        , Url.Parser.map (\string maybeString -> DetailsCommit (maybeStringToFilter maybeString) (Commit string)) (Url.Parser.s "commit" </> Url.Parser.string <?> Url.Parser.Query.string "f")
        , Url.Parser.map (\string maybeString -> DetailsView (maybeStringToFilter maybeString) (View string)) (Url.Parser.s "view" </> Url.Parser.string <?> Url.Parser.Query.string "f")
        , Url.Parser.map (\string maybeString -> Search (maybeStringToFilter maybeString) string) (Url.Parser.s "search" </> Url.Parser.string <?> Url.Parser.Query.string "f")
        , Url.Parser.map f1 (Url.Parser.s "diff" </> Url.Parser.string </> Url.Parser.string <?> Url.Parser.Query.string "f")
        , Url.Parser.map f2 (Url.Parser.s "selected" </> Url.Parser.string <?> Url.Parser.Query.string "f")
        ]


f1 : String -> String -> Maybe String -> Route
f1 screenshotIdAsString1 screenshotIdAsString2 maybeFilterAsString =
    case ( Screenshot.fromString screenshotIdAsString1, Screenshot.fromString screenshotIdAsString2 ) of
        ( Just screenshot1, Just screenshot2 ) ->
            Selected2 (maybeStringToFilter maybeFilterAsString) screenshot1 screenshot2

        _ ->
            Top (maybeStringToFilter maybeFilterAsString)


f2 : String -> Maybe String -> Route
f2 screenshotIdAsString maybeFilterAsString =
    case Screenshot.fromString screenshotIdAsString of
        Just screenshot ->
            Selected1 (maybeStringToFilter maybeFilterAsString) screenshot

        _ ->
            Top (maybeStringToFilter maybeFilterAsString)


maybeStringToFilter : Maybe String -> Filter.Filter
maybeStringToFilter maybeFilterAsString =
    maybeFilterAsString
        |> Filter.fromMaybeString


locationHrefToRoute : String -> Route
locationHrefToRoute locationHref =
    case Url.fromString locationHref of
        Nothing ->
            NotFound Filter.empty (locationHref ++ " is not a valid URL")

        Just url ->
            case Url.Parser.parse urlParser url of
                Nothing ->
                    NotFound Filter.empty ("Route " ++ locationHref ++ " not found")

                Just route ->
                    route


routeToSearchQuery : Route -> Maybe String
routeToSearchQuery route =
    case route of
        Search _ query ->
            Just query

        _ ->
            Nothing



-- ██    ██ ██████  ██████   █████  ████████ ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██
--  ██████  ██      ██████  ██   ██    ██    ███████
--| UPDATE
--|     update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )
        |> andThen updateMain msg
        |> andThen updateErrors msg
        |> andThen updateLocalStorage msg



--|     updateMain


updateMain : Msg -> Model -> ( Model, Cmd Msg )
updateMain msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        MouseMove kind data ->
            if kind == "before" then
                ( { model | offsetXbefore = data.offsetX }, Cmd.none )

            else
                ( { model | offsetXafter = data.offsetX }, Cmd.none )

        MouseLeave ->
            ( { model | offsetXbefore = 10000, offsetXafter = 10000 }, Cmd.none )

        ChangeDiffBlinkingSpeed diffBlinkingSpeed ->
            ( { model | diffBlinkingSpeed = diffBlinkingSpeed }, Cmd.none )

        ChangeLocalStorage localStorageAsString ->
            ( case stringToLocalStorage localStorageAsString of
                Ok localStorage ->
                    localStorageToModel localStorage model

                Err _ ->
                    model
            , Cmd.none
            )

        PushRoute route ->
            ( model, pushUrl <| SharedElement.routeToString route )

        PushRouteAsString string ->
            ( model, pushUrl string )

        ChangeUrl locationHref ->
            --
            -- Message from JavaScript that the url changed
            --
            -- This is either
            --
            -- * The user moved back/forward
            --
            -- * A feedback from sending the command `pushUrl`
            --   (see message `ChangeRoute`) through a port.
            --
            let
                route : Route
                route =
                    locationHrefToRoute locationHref

                filter : Filter.Filter
                filter =
                    routeToFilter route

                isNewFilter : Bool
                isNewFilter =
                    filter /= routeToFilter model.route

                screenshots : Screenshots
                screenshots =
                    case ( isNewFilter, model.httpRequest ) of
                        ( True, Success res ) ->
                            let
                                -- Updating couple of cached values that
                                -- depends on the filter, if the filter
                                -- changed
                                --
                                cache_screenshotsFiltered : List Screenshot.Screenshot
                                cache_screenshotsFiltered =
                                    FilterApplier.filterScreenshots
                                        filter
                                        res.cache_screenshotsAllByCategory
                                        res.response_screenshotsAll

                                cache_screenshotsFilteredByCategory : ScreenshotsByCategory
                                cache_screenshotsFilteredByCategory =
                                    ScreenshotsBy.byCategory
                                        cache_screenshotsFiltered
                            in
                            Success
                                { res
                                    | cache_screenshotsFiltered = cache_screenshotsFiltered
                                    , cache_screenshotsFilteredByCategory = cache_screenshotsFilteredByCategory
                                }

                        _ ->
                            model.httpRequest
            in
            ( { model | route = route, httpRequest = screenshots }
            , ifDiffingSendTheRequestWithPort route
            )

        Response response ->
            let
                newModel : Model
                newModel =
                    modelUpdateHttpRequest response model
            in
            ( newModel
            , ifDiffingSendTheRequestWithPort newModel.route
            )

        ChangePwd pwd ->
            ( { model | pwd = pwd }, ifDiffingSendTheRequestWithPort model.route )

        ToggleInfo ->
            ( { model | infoClosed = not model.infoClosed }, Cmd.none )

        RemoveHttpError ->
            ( { model | httpRequest = Fetching }, Cmd.none )

        EmptyWarnings ->
            -- This is called when an error is closed
            ( { model | warnings = [] }, Cmd.none )


ifDiffingSendTheRequestWithPort : Route -> Cmd Msg
ifDiffingSendTheRequestWithPort route =
    case route of
        Selected2 _ screenshot1 screenshot2 ->
            Cmd.batch
                [ requestDiff
                    ( Screenshot.imageSrcToString <| Screenshot.toImageSrc screenshot1
                    , Screenshot.imageSrcToString <| Screenshot.toImageSrc screenshot2
                    , ( screenshot1.size.x, screenshot1.size.y )
                    )
                , resetViewport
                ]

        _ ->
            Cmd.none



--|     updateErrors


updateErrors : Msg -> Model -> ( Model, Cmd Msg )
updateErrors _ model =
    ( -- Here we transfer errors to the warning system and then we show
      -- the Top page so that users can interact with the app also in
      -- case of errors.
      case model.route of
        NotFound _ string ->
            { model
                | route = Top Filter.empty
                , warnings = string :: model.warnings
            }

        _ ->
            model
    , Cmd.none
    )



--|     updateLocalStorage


updateLocalStorage : Msg -> Model -> ( Model, Cmd Msg )
updateLocalStorage msg model =
    ( model
    , case msg of
        ChangeLocalStorage _ ->
            -- This is the only case where we don't "pushLocalStorage"
            -- to avoid generating an infinite loop
            Cmd.none

        _ ->
            -- We save the model to the local storage
            model
                |> modelToLocalStorage
                |> localStorageToString
                |> pushLocalStorage
    )



-- ██    ██ ██████  ██████   █████  ████████ ███████     ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██    ██ ██   ██ ██   ██ ██   ██    ██    ██          ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ██    ██ ██████  ██   ██ ███████    ██    █████       ███████ █████   ██      ██████  █████   ██████  ███████
-- ██    ██ ██      ██   ██ ██   ██    ██    ██          ██   ██ ██      ██      ██      ██      ██   ██      ██
--  ██████  ██      ██████  ██   ██    ██    ███████     ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
--| UPDATE HELPERS


andThen : (msg -> model -> ( model, Cmd a )) -> msg -> ( model, Cmd a ) -> ( model, Cmd a )
andThen updater msg ( model, cmd ) =
    let
        ( modelNew, cmdNew ) =
            updater msg model
    in
    ( modelNew, Cmd.batch [ cmd, cmdNew ] )


resetViewport : Cmd Msg
resetViewport =
    Task.perform (\_ -> DoNothing) (Browser.Dom.setViewport 0 0)


modelUpdateHttpRequest : Result Http.Error (List Screenshot.Screenshot) -> Model -> Model
modelUpdateHttpRequest response model =
    case response of
        Ok response_screenshotsAll ->
            { model
                | httpRequest =
                    let
                        filter : Filter.Filter
                        filter =
                            routeToFilter model.route

                        cache_screenshotsAllByCategory : ScreenshotsByCategory
                        cache_screenshotsAllByCategory =
                            ScreenshotsBy.byCategory response_screenshotsAll

                        cache_screenshotsFiltered : List Screenshot.Screenshot
                        cache_screenshotsFiltered =
                            FilterApplier.filterScreenshots
                                filter
                                cache_screenshotsAllByCategory
                                response_screenshotsAll

                        cache_screenshotsFilteredByCategory : ScreenshotsByCategory
                        cache_screenshotsFilteredByCategory =
                            ScreenshotsBy.byCategory cache_screenshotsFiltered
                    in
                    Success
                        { response_screenshotsAll = response_screenshotsAll
                        , cache_screenshotsAllByCategory = cache_screenshotsAllByCategory
                        , cache_screenshotsFiltered = cache_screenshotsFiltered
                        , cache_screenshotsFilteredByCategory = cache_screenshotsFilteredByCategory
                        }
            }

        Err error ->
            { model | httpRequest = Failure <| httpErrorToString error }



-- ██████   ██████  ██████  ████████ ███████
-- ██   ██ ██    ██ ██   ██    ██    ██
-- ██████  ██    ██ ██████     ██    ███████
-- ██      ██    ██ ██   ██    ██         ██
-- ██       ██████  ██   ██    ██    ███████
--| PORTS


port pushLocalStorage : String -> Cmd msg


port pushUrl : String -> Cmd msg


port requestDiff : ( String, String, ( Int, Int ) ) -> Cmd msg


port onLocalStorageChange : (String -> msg) -> Sub msg


port onUrlChange : (String -> msg) -> Sub msg



-- ███    ███  █████  ██ ███    ██
-- ████  ████ ██   ██ ██ ████   ██
-- ██ ████ ██ ███████ ██ ██ ██  ██
-- ██  ██  ██ ██   ██ ██ ██  ██ ██
-- ██      ██ ██   ██ ██ ██   ████
--| MAIN


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view =
            \model ->
                { title =
                    if Maybe.withDefault "" (routeToSearchQuery model.route) == "" then
                        configuration.nameWebsite

                    else
                        "\"" ++ Maybe.withDefault "" (routeToSearchQuery model.route) ++ "\" " ++ configuration.nameWebsite
                , body = [ view configuration model ]
                }
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : a -> Sub Msg
subscriptions _ =
    Sub.batch
        [ onUrlChange ChangeUrl
        , onLocalStorageChange ChangeLocalStorage
        ]



-- ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ███████ █████   ██      ██████  █████   ██████  ███████
-- ██   ██ ██      ██      ██      ██      ██   ██      ██
-- ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
--| GENERIC HELPERS


commandToRequestScreenshots : Cmd Msg
commandToRequestScreenshots =
    Http.get
        { url = "/dir.json"
        , expect = Http.expectJson Response decoderListProducts
        }


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl string ->
            "Bad URL " ++ string

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus status ->
            "Bad Status " ++ String.fromInt status

        Http.BadBody string ->
            string



--  █████  ████████  ██████  ███    ███ ███████
-- ██   ██    ██    ██    ██ ████  ████ ██
-- ███████    ██    ██    ██ ██ ████ ██ ███████
-- ██   ██    ██    ██    ██ ██  ██  ██      ██
-- ██   ██    ██     ██████  ██      ██ ███████
--| ATOMS
--|     atomLogo


atomLogo : List (AttributeC msg) -> { b | color : Color, size : Float } -> ElementC msg
atomLogo attrs args =
    row
        ([ SharedElement.tag "atomLogo"
         , spacing (round (args.size / 3))
         , Font.size (round (args.size * 3))
         , Font.light
         , Font.color args.color
         , Font.family [ Font.typeface "Big Shoulders Display" ]
         ]
            ++ attrs
        )
        [ text "Image"
        , el [ Font.size (round (args.size * 3)), moveUp 6 ] <| text "🦉"
        , text "Diff"
        ]



--|     atomIcon


atomIcon :
    List (AttributeC msg)
    -> { color : Palette -> Color, fill : Bool, shape : FeatherIcons.Icon, size : Float }
    -> ElementC msg
atomIcon attrs { shape, color, size, fill } =
    withContext <|
        \c ->
            el attrs <|
                html <|
                    (shape
                        |> FeatherIcons.withSize size
                        |> FeatherIcons.withStrokeWidth 1
                        |> FeatherIcons.toHtml
                            (Html.Attributes.style "stroke" (colorToCssString (color c.palette))
                                :: (if fill && shape /= FeatherIcons.arrowUp && shape /= FeatherIcons.chevronLeft && shape /= FeatherIcons.chevronRight then
                                        [ Html.Attributes.style "fill" (colorToCssString (changeAlpha 1 (color c.palette))) ]
                                        -- [ Html.Attributes.style "fill" "rgb(255, 111 ,255)" ]

                                    else
                                        []
                                   )
                            )
                    )



-- ███    ███  ██████  ██      ███████  ██████ ██    ██ ██      ███████ ███████
-- ████  ████ ██    ██ ██      ██      ██      ██    ██ ██      ██      ██
-- ██ ████ ██ ██    ██ ██      █████   ██      ██    ██ ██      █████   ███████
-- ██  ██  ██ ██    ██ ██      ██      ██      ██    ██ ██      ██           ██
-- ██      ██  ██████  ███████ ███████  ██████  ██████  ███████ ███████ ███████
--| MOLECULES
--|     molProductNotFound
--|     molClose


molSecondaryButton : List (AttributeC msg) -> FeatherIcons.Icon -> msg -> ElementC msg
molSecondaryButton attrs shape msg =
    Input.button
        ([ SharedElement.tag "molSecondaryButton"
         , title "Close"
         , Border.rounded 50
         , Border.width 1
         , width <| px 40
         , height <| px 40
         , SharedElement.attrWithContext <| \c -> Background.color c.palette.surface
         , SharedElement.attrWithContext <| \c -> Border.color c.palette.secondary
         , SharedElement.transition "box-shadow 0.2s, background-color 0.2s, transform 0.6s"
         , mouseOver
            [ shadowHigh
            , decorationWithContext <| \c -> Background.color c.palette.surface2dp
            ]
         ]
            ++ attrs
        )
        { label =
            atomIcon
                [ centerX
                , centerY
                , SharedElement.transition "transform 0.4s"
                , mouseOver [ scale 1.2 ]
                ]
                { shape = shape
                , color = .secondary
                , size = 30
                , fill = False
                }
        , onPress = Just <| msg
        }



--  ██████  ██████   ██████   █████  ███    ██ ██ ███████ ███    ███ ███████
-- ██    ██ ██   ██ ██       ██   ██ ████   ██ ██ ██      ████  ████ ██
-- ██    ██ ██████  ██   ███ ███████ ██ ██  ██ ██ ███████ ██ ████ ██ ███████
-- ██    ██ ██   ██ ██    ██ ██   ██ ██  ██ ██ ██      ██ ██  ██  ██      ██
--  ██████  ██   ██  ██████  ██   ██ ██   ████ ██ ███████ ██      ██ ███████
--| ORGANISMS
--|     orgHeader
--|     orgFooter


orgFooter : ElementC Msg
orgFooter =
    column
        [ SharedElement.tag "orgFooter"
        , attrWidth
        , centerX
        , Font.center
        , paddingXY 20 40
        , spacing 20
        , inFront <|
            html <|
                Html.node "style" [] [ Html.text """@keyframes pulse {
                        0% {
                            transform: scale(0.9);
                            box-shadow: 0 0 0 0 rgba(255, 255, 255, 0.4);
                        }

                        50% {
                            transform: scale(0.9);
                            box-shadow: 0 0 0 0 rgba(255, 255, 255, 0.4);
                        }

                        80% {
                            transform: scale(1.1);
                            box-shadow: 0 0 10px 10px rgba(255, 255, 255, 0);
                        }

                        100% {
                            transform: scale(0.9);
                            box-shadow: 0 0 0 0 rgba(255, 255, 255, 0);
                        }
                    }""" ]
        ]
        [ column [ padding 10, centerX, spacing 10, Font.size 13 ]
            [ row [ SharedElement.attrWithContext <| \c -> Font.color c.palette.onExternal ]
                [ text "Powered by elm-image-diff 🦉" ]
            ]
        ]



--|     orgError


orgError : msg -> List (ElementC msg) -> ElementC msg
orgError msg content =
    column
        [ SharedElement.tag "orgError"
        , alignBottom
        , width fill
        , SharedElement.attrWithContext <| \c -> Background.color c.palette.secondary
        , SharedElement.attrWithContext <| \c -> Font.color c.palette.onSecondary
        , inFront <| molSecondaryButton [ moveUp 20, moveLeft 40, alignRight ] FeatherIcons.x msg
        , paddingEach { top = 25, right = 10, bottom = 10, left = 10 }
        , spacing 15
        , Border.shadow { offset = ( 0, 0 ), size = 2, blur = 10, color = rgba 0 0 0 0.1 }
        ]
        content



-- ██████   █████  ██      ███████ ████████ ████████ ███████
-- ██   ██ ██   ██ ██      ██         ██       ██    ██
-- ██████  ███████ ██      █████      ██       ██    █████
-- ██      ██   ██ ██      ██         ██       ██    ██
-- ██      ██   ██ ███████ ███████    ██       ██    ███████
--| PALETTE
--|     paletteLight


paletteLight : Palette
paletteLight =
    { primary = rgb255 17 147 215
    , secondary = rgb255 80 181 237
    , external =
        if styleWebsite == Unbranded then
            -- rgb255 125 0 190
            rgb255 100 105 110

        else
            rgb255 17 147 215
    , background = rgb255 245 245 245
    , surface = rgb255 250 250 250
    , surface2dp = rgb255 255 255 255
    , onExternal = rgb255 255 255 255
    , onPrimary = rgb255 255 255 255
    , onSecondary = rgb255 255 255 255
    , onBackground = rgb255 255 255 255
    , onBackgroundDim = rgb255 100 100 100
    , mask = rgb255 235 235 235
    , separator = rgb255 200 200 200
    , error = rgb255 200 0 0
    }



--|     paletteDark


paletteDark : Palette
paletteDark =
    { primary = rgb255 255 255 255
    , secondary = rgb255 17 147 215
    , external = rgb255 6 103 154
    , background = rgb255 20 20 20
    , surface = rgb255 50 50 50
    , surface2dp = rgb255 60 60 60
    , onExternal = rgb255 255 255 255
    , onPrimary = rgb255 0 0 0
    , onSecondary = rgb255 255 255 255
    , onBackground = rgb255 230 230 230
    , onBackgroundDim = rgb255 180 180 180
    , mask = rgb255 100 100 100
    , separator = rgb255 110 110 110
    , error = rgb255 255 100 100
    }



-- ██    ██ ██ ███████ ██     ██ ███████
-- ██    ██ ██ ██      ██     ██ ██
-- ██    ██ ██ █████   ██  █  ██ ███████
--  ██  ██  ██ ██      ██ ███ ██      ██
--   ████   ██ ███████  ███ ███  ███████
--| VIEWS
--|     view


type Access
    = Granted
    | RequirePassword
    | NotGranted


isAccessible : Model -> Access
isAccessible model =
    case model.maybeHashedLocationHref of
        Nothing ->
            NotGranted

        Just hashedLocationHref ->
            if
                hashedLocationHref
                    -- http://localhost:5924
                    == "2a1556133d3296c08fb26c05a4204abab7094498e93fb29fbadfed53"
                    || hashedLocationHref
                    -- https://54he49.surge.sh
                    == "0c8a968a17bf2b7b00ac734ec498d7088a72d95b78de57f7df654f46"
                    || hashedLocationHref
                    -- https://54he49.netlify.app
                    == "4d8f2c9a3b96e4416ff9d90f39ae098e7fce812e5f6720a841e61336"
            then
                case model.maybeHashedFpInLocalStorage of
                    Just hashedFpInLocalStorage ->
                        if hashedFpInLocalStorage == model.hashedFp then
                            -- User already logged in with this browser
                            Granted

                        else
                            -- Local storage is wrong, trying regular password
                            isCorrectPwd model

                    Nothing ->
                        -- No local storage, trying regular password
                        isCorrectPwd model

            else
                NotGranted


masha : String -> String
masha string =
    string
        |> (++) "2587c395c415a53199ce7261da9c1379ac65172232b9fe16a1059309"
        |> String.reverse
        |> Crypto.Hash.sha224
        |> String.reverse


isCorrectPwd : Model -> Access
isCorrectPwd model =
    let
        hashedPwd : String
        hashedPwd =
            masha model.pwd
    in
    if
        hashedPwd
            -- d..3
            == "a14fbb11e27fd7a90d358e3b50293327346791042aeeb2990941b30a"
            || hashedPwd
            -- j9e3jfndsd734mcd3284e
            == "c4861c954a4e88d146b092ebb1cd9be2490bca8f6608c46e482f66a9"
    then
        Granted

    else
        RequirePassword


view : Configuration -> Model -> Html.Html Msg
view conf model =
    case isAccessible model of
        Granted ->
            case model.httpRequest of
                Fetching ->
                    Html.text "Fetching"

                Failure failure ->
                    Html.text <| "Failure: " ++ failure

                Success res ->
                    layoutWith (contextBuilder conf model.mode model.language (routeToFilter model.route))
                        { options = options }
                        (mainAttrs conf model)
                        (column
                            [ width fill
                            , spacing 20
                            , alignTop
                            , padding 20
                            ]
                            ([]
                                ++ [ ViewHeader.view model ]
                                ++ (case model.route of
                                        Top _ ->
                                            [ ViewTop.view model res ]

                                        Latest _ ->
                                            ViewLatest.view model res

                                        History _ ->
                                            ViewHistory.view model res Nothing

                                        Selected1 _ screenshot ->
                                            ViewHistory.view model res (Just screenshot)

                                        Selected2 _ screenshot1 screenshot2 ->
                                            ViewDiff.view model screenshot1 screenshot2

                                        DetailsImage _ _ ->
                                            [ text "details" ]

                                        DetailsCommit _ _ ->
                                            [ text "details commit" ]

                                        DetailsView _ _ ->
                                            [ text "details view" ]

                                        Search _ _ ->
                                            [ text "search" ]

                                        NotFound _ err ->
                                            [ paragraph [] [ text err ] ]
                                   )
                                ++ [ orgFooter ]
                                ++ [ css ]
                                ++ [ cssDebugging ]
                            )
                        )

        RequirePassword ->
            layoutWith (contextBuilder conf model.mode model.language (routeToFilter model.route))
                { options = options }
                []
                (viewPwd model.pwd)

        NotGranted ->
            Html.div
                [ Html.Attributes.style "display" "flex"
                , Html.Attributes.style "justify-content" "center"
                , Html.Attributes.style "align-items" "center"
                , Html.Attributes.style "height" "100vh"
                , Html.Attributes.style "font-size" "30px"
                , Html.Attributes.style "background-color" "rgba(0,0,0,0.7)"
                ]
                [ Html.text "🔒" ]



-- ██    ██ ██ ███████ ██     ██     ██   ██ ███████ ██      ██████  ███████ ██████  ███████
-- ██    ██ ██ ██      ██     ██     ██   ██ ██      ██      ██   ██ ██      ██   ██ ██
-- ██    ██ ██ █████   ██  █  ██     ███████ █████   ██      ██████  █████   ██████  ███████
--  ██  ██  ██ ██      ██ ███ ██     ██   ██ ██      ██      ██      ██      ██   ██      ██
--   ████   ██ ███████  ███ ███      ██   ██ ███████ ███████ ██      ███████ ██   ██ ███████
--| VIEW HELPERS


inFrontErrors : String -> ElementC Msg
inFrontErrors error =
    orgError RemoveHttpError <|
        [ paragraph [] [ text "Problem with the API reponse, try reloading the page" ]
        , el [ scrollbarX, width fill ] <|
            el
                []
            <|
                html <|
                    Html.pre [] [ Html.text error ]
        ]


inFrontWarnings : List String -> ElementC Msg
inFrontWarnings warnings =
    orgError EmptyWarnings <|
        List.map (\string -> paragraph [ Font.center, SharedElement.style "word-break" "break-all" ] [ text string ]) warnings


inFrontInfoModal : ElementC Msg
inFrontInfoModal =
    el
        [ width fill
        , height fill
        , inFront <|
            el
                [ width fill
                , height fill
                , Background.color <| rgba 0 0 0 0.3
                , Events.onClick ToggleInfo
                ]
                none
        , inFront <|
            el
                [ centerX
                , centerY
                , padding 20
                , inFront <|
                    molSecondaryButton
                        [ moveLeft 40
                        , alignRight
                        ]
                        FeatherIcons.x
                        ToggleInfo
                ]
            <|
                column
                    [ SharedElement.attrWithContext <| \c -> Background.color c.palette.external
                    , SharedElement.attrWithContext <| \c -> Font.color c.palette.onExternal
                    , shadowHigh
                    , Border.rounded 10
                    , padding 30
                    , spacing 20
                    , height <| px 350
                    , width (fill |> maximum 400)
                    , scrollbarY
                    ]
                    [ withContext <| \c -> atomLogo [ centerX ] { size = 28, color = c.palette.onSecondary }
                    , paragraph [ Font.center ]
                        [ text "Blah blah blah"
                        ]
                    ]
        ]
        none


options : List Option
options =
    [ focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing } ]


colorToCssString : Color -> String
colorToCssString color =
    -- Copied from https://github.com/avh4/elm-color/blob/1.0.0/src/Color.elm#L555
    let
        { red, green, blue, alpha } =
            toRgb color

        pct : Float -> Float
        pct x =
            ((x * 10000) |> round |> toFloat) / 100

        roundTo : Float -> Float
        roundTo x =
            ((x * 1000) |> round |> toFloat) / 1000
    in
    String.concat
        [ "rgba("
        , String.fromFloat (pct red)
        , "%,"
        , String.fromFloat (pct green)
        , "%,"
        , String.fromFloat (pct blue)
        , "%,"
        , String.fromFloat (roundTo alpha)
        , ")"
        ]


changeAlpha : Float -> Color -> Color
changeAlpha alpha color =
    let
        { red, green, blue } =
            toRgb color
    in
    rgba red green blue alpha


css : ElementC msg
css =
    -- This CSS is to remove the blue highlight when a button is pressed,
    -- in Chrome for Android and to fix a regression in elm-ui 1.1.8
    html <| Html.node "style" [] [ Html.text """div[role=button] { -webkit-tap-highlight-color: transparent} .s.c > .s {flex-basis: auto}
.s.r > s:first-of-type.accx { flex-grow: 0 !important; }
.s.r > s:last-of-type.accx { flex-grow: 0 !important; }
.cx > .wrp { justify-content: center !important; }    
""" ]


viewPwd : String -> ElementC Msg
viewPwd pwd =
    el
        [ width fill
        , height fill
        , Background.color <| rgb 0.2 0.2 0.2
        , Font.color <| rgba 1 1 1 0.8
        ]
    <|
        Input.currentPassword
            [ centerX
            , centerY
            , width (fill |> maximum 200)
            , Background.color <| rgba 0 0 0 0.8
            , Border.width 0
            ]
            { label = Input.labelLeft [ Font.size 28, moveDown 4 ] <| text "🔒"
            , onChange = ChangePwd
            , placeholder = Nothing
            , text = pwd
            , show = False
            }



-- ██    ██ ██ ███████ ██     ██      █████  ████████ ████████ ██████  ██ ██████  ██    ██ ████████ ███████ ███████
-- ██    ██ ██ ██      ██     ██     ██   ██    ██       ██    ██   ██ ██ ██   ██ ██    ██    ██    ██      ██
-- ██    ██ ██ █████   ██  █  ██     ███████    ██       ██    ██████  ██ ██████  ██    ██    ██    █████   ███████
--  ██  ██  ██ ██      ██ ███ ██     ██   ██    ██       ██    ██   ██ ██ ██   ██ ██    ██    ██    ██           ██
--   ████   ██ ███████  ███ ███      ██   ██    ██       ██    ██   ██ ██ ██████   ██████     ██    ███████ ███████
--| VIEW ATTRIBUTES
--|     mainAttrs


mainAttrs : Configuration -> Model -> List (AttributeC Msg)
mainAttrs conf model =
    let
        c : Context
        c =
            contextBuilder conf model.mode model.language (routeToFilter model.route)
    in
    [ SharedElement.tag "mainAttrs"
    , Font.size 16
    , Font.color c.palette.onBackground
    , Font.family
        [ Font.typeface "IBM Plex Sans"
        , Font.sansSerif
        ]
    , Font.light
    , SharedElement.transition "background-color 2s"
    , Background.color c.palette.external

    -- In front stuff
    , if model.infoClosed || styleWebsite == Unbranded then
        SharedElement.noneAttr

      else
        inFront <| inFrontInfoModal
    , if model.warnings == [] then
        SharedElement.noneAttr

      else
        inFront <| inFrontWarnings model.warnings
    , case model.httpRequest of
        Failure error ->
            inFront <| inFrontErrors error

        _ ->
            SharedElement.noneAttr
    ]


title : String -> AttributeC msg
title string =
    htmlAttribute <| Html.Attributes.title string


shadowHigh : AttrC decorative msg
shadowHigh =
    Border.shadow { offset = ( 0, 2 ), size = 0, blur = 10, color = rgba 0 0 0 0.2 }


attrWidth : AttributeC msg
attrWidth =
    SharedElement.attrWithContext <| \c -> width (fill |> maximum c.conf.maxWidth)



--| DEBUGGING


cssDebugging : ElementC msg
cssDebugging =
    withContext <|
        \c ->
            if c.conf.debugging then
                -- https://css-tricks.com/a-complete-guide-to-data-attributes/
                html <| Html.node "style" [] [ Html.text """[data-SharedElement.tag]:hover:after { opacity: 1; } [data-SharedElement.tag]::after { SharedElement.transition: opacity 0.2s;  opacity: 0.2; content: attr(data-SharedElement.tag); background-color: #ff0; z-index: 100; color: #660; border: 1px solid #660; border-radius: 10px 0 0 0; position: absolute; bottom: 0; right: 0; font-size: 13px; padding: 2px 2px 2px 4px; } [data-SharedElement.tag^="atom"]::after { background-color: #faf; } [data-SharedElement.tag^="org"]::after { background-color: #0f0; } [data-SharedElement.tag^="mol"]::after { background-color: #aaf; }""" ]

            else
                none



--| LOCAL STORAGE


localStorageToModel : LocalStorage -> Model -> Model
localStorageToModel localStorage model =
    { model
        | mode = localStorage.mode
        , language = localStorage.language
        , maybeHashedFpInLocalStorage = localStorage.hashedFp
        , infoClosed = localStorage.infoClosed
        , diffBlinkingSpeed = localStorage.diffBlinkingSpeed
    }


modelToLocalStorage : Model -> LocalStorage
modelToLocalStorage model =
    { mode = model.mode
    , language = model.language
    , hashedFp =
        case isAccessible model of
            Granted ->
                Just <| model.hashedFp

            RequirePassword ->
                Nothing

            NotGranted ->
                Nothing
    , infoClosed = model.infoClosed
    , diffBlinkingSpeed = model.diffBlinkingSpeed
    }



--  ██████  ██████  ██████  ███████  ██████ ███████
-- ██      ██    ██ ██   ██ ██      ██      ██
-- ██      ██    ██ ██   ██ █████   ██      ███████
-- ██      ██    ██ ██   ██ ██      ██           ██
--  ██████  ██████  ██████  ███████  ██████ ███████
--| CODECS
--
-- We use the library miniBill/elm-codec instead using directly elm/json
-- conders and decoders because with elm-codec we only need to write one
-- thing.
-- For more info, see:
-- https://package.elm-lang.org/packages/miniBill/elm-codec/latest/


f : String -> Codec.Codec Screenshot.Screenshot
f string =
    case Screenshot.fromString string of
        Just screenshot ->
            Codec.succeed screenshot

        Nothing ->
            Codec.fail (string ++ " is not a valid screenshot")


codecProduct : Codec.Codec Screenshot.Screenshot
codecProduct =
    Codec.string
        |> Codec.andThen f (Screenshot.toImageSrc >> Screenshot.imageSrcToString)


decoderListProducts : Json.Decode.Decoder (List Screenshot.Screenshot)
decoderListProducts =
    Codec.decoder (Codec.list codecProduct)


stringToLocalStorage : String -> Result Codec.Error LocalStorage
stringToLocalStorage string =
    Codec.decodeString codecModel string


localStorageToString : LocalStorage -> String
localStorageToString model =
    Codec.encodeToString 4 codecModel model


codecModel : Codec.Codec LocalStorage
codecModel =
    Codec.object LocalStorage
        |> Codec.field "mode" .mode codecMode
        |> Codec.field "language" .language codecLanguage
        |> Codec.field "hfp" .hashedFp (Codec.maybe Codec.string)
        |> Codec.field "infoClosed" .infoClosed Codec.bool
        |> Codec.field "diffBlinkingSpeed" .diffBlinkingSpeed Codec.int
        |> Codec.buildObject


codecMode : Codec.Codec Mode
codecMode =
    Codec.custom
        (\light dark value ->
            case value of
                Light ->
                    light

                Dark ->
                    dark
        )
        |> Codec.variant0 "Light" Light
        |> Codec.variant0 "Dark" Dark
        |> Codec.buildCustom


codecLanguage : Codec.Codec Language
codecLanguage =
    Codec.custom
        (\enUS jaJP value ->
            case value of
                EN_US ->
                    enUS

                JA_JP ->
                    jaJP
        )
        |> Codec.variant0 "en_US" EN_US
        |> Codec.variant0 "ja_JP" JA_JP
        |> Codec.buildCustom
