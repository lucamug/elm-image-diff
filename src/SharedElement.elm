module SharedElement exposing (..)

import Context exposing (..)
import DateFormat.Relative
import Element.WithContext exposing (..)
import Element.WithContext.Background as Background
import Element.WithContext.Border as Border
import Element.WithContext.Font as Font
import Filter
import Html
import Html.Attributes
import Html.Events
import Iso8601
import Json.Decode
import Parser
import Screenshot
import Shared exposing (..)
import SharedTypes exposing (..)
import Time
import Url.Builder


viewCanvas :
    List (Attribute context msg)
    ->
        { b
            | id : String
            , screenSize : { a | x : Int, y : Int }
            , widthPreview : Int
        }
    -> Element context msg
viewCanvas attrs { widthPreview, screenSize, id } =
    el
        ([ width <| px widthPreview
         , Border.rounded 10
         , Border.shadow { offset = ( 0, 0 ), size = 0, blur = 10, color = rgba 0 0 0 0.2 }
         , clip
         ]
            ++ attrs
        )
    <|
        html <|
            Html.canvas
                [ Html.Attributes.id id
                , Html.Attributes.width screenSize.x
                , Html.Attributes.height screenSize.y
                ]
                []


innerLabel : String -> Element context msg
innerLabel string =
    el
        [ paddingXY 10 5
        , Background.color <|
            if string == "+ ADDED" then
                rgba255 99 190 122 1

            else if string == "- DELETED" then
                rgba255 222 126 134 1

            else
                rgba255 180 180 180 1
        , Border.rounded 5
        , Font.color <| rgb 1 1 1
        , Font.size 16
        , Font.bold
        ]
    <|
        text string


attrWithContext : (Context -> Attribute Context msg) -> Attribute Context msg
attrWithContext =
    withAttribute identity


preventDefault : msg -> Attribute context msg
preventDefault msg =
    -- From https://github.com/elm/browser/blob/1.0.2/notes/navigation-in-elements.md
    htmlAttribute <|
        Html.Events.preventDefaultOn
            "click"
            (Json.Decode.succeed ( msg, True ))


routeToString : Route -> String
routeToString route =
    let
        ( path, filter_ ) =
            case route of
                Top filter ->
                    ( [], filter )

                Latest filter ->
                    ( [ "latest" ], filter )

                History filter ->
                    ( [ "history" ], filter )

                DetailsImage filter (Image imageAsString) ->
                    ( [ "image", imageAsString ], filter )

                DetailsCommit filter (Commit commitAsString) ->
                    ( [ "commit", commitAsString ], filter )

                DetailsView filter (View viewAsString) ->
                    ( [ "image", viewAsString ], filter )

                Selected2 filter screenshot1 screenshot2 ->
                    ( [ "diff"
                      , Screenshot.idToString <| Screenshot.toId screenshot1
                      , Screenshot.idToString <| Screenshot.toId screenshot2
                      ]
                    , filter
                    )

                Selected1 filter screenshot ->
                    ( [ "selected", Screenshot.idToString <| Screenshot.toId screenshot ], filter )

                Search filter query ->
                    ( [ "search", query ], filter )

                NotFound filter _ ->
                    ( [ "not_found" ], filter )
    in
    Url.Builder.absolute path
        (case Filter.toMaybeString filter_ of
            Nothing ->
                []

            Just filterAsString ->
                [ Url.Builder.string "f" filterAsString ]
        )


atomLinkInternal :
    List (Attribute Context Msg)
    -> { label : Element Context Msg, route : Route }
    -> Element Context Msg
atomLinkInternal attrs args =
    link
        ([ tag "atomLinkInternal", preventDefault (PushRoute args.route) ] ++ attrs)
        { label = args.label
        , url = routeToString args.route
        }


atomLinkInternalWithUrlAsString :
    List (Attribute Context Msg)
    -> { label : Element Context Msg, url : String }
    -> Element Context Msg
atomLinkInternalWithUrlAsString attrs args =
    link
        ([ tag "atomLinkInternal", preventDefault (PushRouteAsString args.url) ] ++ attrs)
        { label = args.label
        , url = args.url
        }


noneAttr : Attribute context msg
noneAttr =
    htmlAttribute <| Html.Attributes.style "" ""


tag : String -> Attribute Context msg
tag string =
    attrWithContext <|
        \c ->
            if c.conf.debugging then
                htmlAttribute <| Html.Attributes.attribute "data-tag" string

            else
                noneAttr


stringToPosixAndCommit : String -> Maybe { commit : String, posix : Time.Posix }
stringToPosixAndCommit string =
    string
        |> String.split "_"
        |> (\list ->
                case list of
                    date :: commit :: _ ->
                        let
                            resultPosix : Result (List Parser.DeadEnd) Time.Posix
                            resultPosix =
                                Iso8601.toTime <| String.replace "." ":" date
                        in
                        case resultPosix of
                            Ok posix ->
                                Just
                                    { posix = posix
                                    , commit = commit
                                    }

                            Err _ ->
                                Nothing

                    _ ->
                        Nothing
           )


humanDateAndCommit : Int -> String -> Element context msg
humanDateAndCommit millisNow string =
    string
        |> stringToPosixAndCommit
        |> Maybe.map
            (\{ posix, commit } ->
                row [ spacing 20 ]
                    [ el [] <| text <| DateFormat.Relative.relativeTime (Time.millisToPosix millisNow) posix
                    , el [ alpha 0.5 ] <| text <| commit
                    , el [ alpha 0.5 ] <| text <| dateFormatter Time.utc posix
                    ]
            )
        |> Maybe.withDefault (text string)


humanDateAndCommitShort : String -> Element context msg
humanDateAndCommitShort string =
    if string == "" then
        text "N/A"

    else
        string
            |> stringToPosixAndCommit
            |> Maybe.map
                (\{ posix, commit } ->
                    row [ spacing 10 ]
                        [ text <| dateFormatterShort Time.utc posix
                        , text <| String.left 4 commit
                        ]
                )
            |> Maybe.withDefault (text string)


buttonAttrs : Bool -> List (Attribute context msg)
buttonAttrs bool =
    [ Border.width 1
    , Border.rounded 5
    , Border.color <| rgba 1 1 1 0.3
    , Background.color <|
        if bool then
            rgba 0 0 0 0.2

        else
            rgba 0 0 0 0
    , mouseOver [ Background.color <| rgba 0 0 0 0.2 ]
    , paddingXY 16 8
    , style "letter-spacing" "2px"
    ]


style : String -> String -> Attribute context msg
style string1 string2 =
    htmlAttribute <| Html.Attributes.style string1 string2


transition : String -> Attribute context msg
transition string =
    style "transition" string
