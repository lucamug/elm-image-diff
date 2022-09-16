module Context exposing (..)

import Element.WithContext exposing (..)
import Filter


type alias Palette =
    { primary : Color
    , secondary : Color
    , external : Color
    , background : Color
    , surface : Color
    , surface2dp : Color
    , onExternal : Color
    , onPrimary : Color
    , onSecondary : Color
    , onBackground : Color
    , onBackgroundDim : Color
    , separator : Color
    , mask : Color
    , error : Color
    }


palette : Mode -> { b | paletteDark : a, paletteLight : a } -> a
palette mode conf =
    case mode of
        Light ->
            conf.paletteLight

        Dark ->
            conf.paletteDark


type alias Configuration =
    { nameWebsite : String
    , mediaLocation : String
    , gridMinimum : Int
    , gridMaximum : Int
    , maxWidth : Int
    , debugging : Bool
    , paletteLight : Palette
    , paletteDark : Palette
    }


type Language
    = EN_US
    | JA_JP


type Mode
    = Light
    | Dark



--  ██████  ██████  ███    ██ ████████ ███████ ██   ██ ████████
-- ██      ██    ██ ████   ██    ██    ██       ██ ██     ██
-- ██      ██    ██ ██ ██  ██    ██    █████     ███      ██
-- ██      ██    ██ ██  ██ ██    ██    ██       ██ ██     ██
--  ██████  ██████  ██   ████    ██    ███████ ██   ██    ██
--| CONTEXT
--
-- We use miniBill/elm-ui-with-context, an extension of mdgriffith/elm-ui
-- to facilitate passing some value where in needed down the tree of views.
-- For more info, see:
-- https://package.elm-lang.org/packages/miniBill/elm-ui-with-context/latest/


type alias Context =
    -- Context contain data that is needed to draw the views but it doesn't
    -- change much. It include the entire configuration that doesn't change
    -- at all during the life of the application.
    { conf : Configuration
    , palette : Palette
    , language : Language

    -- Filter is needed to create all links in the app
    , filter : Filter.Filter
    }


type alias ElementC msg =
    Element.WithContext.Element Context msg


type alias AttributeC msg =
    Element.WithContext.Attribute Context msg


type alias AttrC decorative msg =
    Element.WithContext.Attr Context decorative msg


withContext : (Context -> ElementC msg) -> ElementC msg
withContext =
    with identity


decorationWithContext : (msg -> Element.WithContext.Decoration msg) -> Element.WithContext.Decoration msg
decorationWithContext =
    withDecoration identity


contextBuilder : Configuration -> Mode -> Language -> Filter.Filter -> Context
contextBuilder conf mode language filter =
    { conf = conf
    , palette = palette mode conf
    , language = language
    , filter = filter
    }
