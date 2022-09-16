module ViewHeader exposing (view)

import Context exposing (..)
import Element.WithContext exposing (..)
import SharedElement exposing (..)
import SharedTypes exposing (..)


view : { a | route : Route } -> Element Context Msg
view model =
    row [ spacing 20 ] <|
        []
            ++ [ let
                    route : Route
                    route =
                        Top (routeToFilter model.route)
                 in
                 atomLinkInternal
                    (buttonAttrs (route == model.route))
                    { label = text "TOP"
                    , route = route
                    }
               ]
            ++ [ let
                    route : Route
                    route =
                        Latest (routeToFilter model.route)
                 in
                 atomLinkInternal
                    (buttonAttrs (route == model.route))
                    { label = text "LATEST"
                    , route = route
                    }
               ]
            ++ [ let
                    route : Route
                    route =
                        History (routeToFilter model.route)
                 in
                 atomLinkInternal
                    (buttonAttrs (route == model.route))
                    { label = text "HISTORY"
                    , route = route
                    }
               ]
