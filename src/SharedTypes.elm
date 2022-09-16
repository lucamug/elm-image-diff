module SharedTypes exposing (..)

import Context exposing (..)
import Dict
import Element.WithContext exposing (..)
import Filter
import Http
import Screenshot
import Set


type alias Model =
    -- Saved in Local Storage
    { mode : Mode
    , language : Language
    , pwd : String
    , infoClosed : Bool
    , route : Route
    , httpRequest : Screenshots
    , warnings : List String
    , hideOverview : Bool
    , posix : Int
    , diffBlinkingSpeed : Int
    , hashedFp : String
    , maybeHashedLocationHref : Maybe String
    , maybeHashedFpInLocalStorage : Maybe String
    , offsetXbefore : Int
    , offsetXafter : Int
    }


type Screenshots
    = Fetching
    | Failure String
    | Success ResponseAndCache


type alias ResponseAndCache =
    { response_screenshotsAll : List Screenshot.Screenshot
    , cache_screenshotsAllByCategory : ScreenshotsByCategory
    , cache_screenshotsFiltered : List Screenshot.Screenshot
    , cache_screenshotsFilteredByCategory : ScreenshotsByCategory
    }


type alias ScreenshotsByCategory =
    { size : Dict.Dict String (Set.Set String)
    , cat2 : Dict.Dict String (Set.Set String)
    , cat3 : Dict.Dict String (Set.Set String)
    , cat4 : Dict.Dict String (Set.Set String)
    , cat5 : Dict.Dict String (Set.Set String)
    , cat6 : Dict.Dict String (Set.Set String)
    , cat7 : Dict.Dict String (Set.Set String)
    , cat8 : Dict.Dict String (Set.Set String)
    , cat9 : Dict.Dict String (Set.Set String)
    , cat10 : Dict.Dict String (Set.Set String)
    , time : Dict.Dict String (Set.Set String)
    }


changeFilterOfRoute : Filter.Filter -> Route -> Route
changeFilterOfRoute filter route =
    case route of
        Top _ ->
            Top filter

        Latest _ ->
            Latest filter

        History _ ->
            History filter

        NotFound _ a ->
            NotFound filter a

        DetailsImage _ a ->
            DetailsImage filter a

        DetailsCommit _ a ->
            DetailsCommit filter a

        DetailsView _ a ->
            DetailsView filter a

        Selected1 _ a ->
            Selected1 filter a

        Selected2 _ a b ->
            Selected2 filter a b

        Search _ a ->
            Search filter a


routeToFilter : Route -> Filter.Filter
routeToFilter route =
    case route of
        Top filter ->
            filter

        Latest filter ->
            filter

        History filter ->
            filter

        NotFound filter _ ->
            filter

        DetailsImage filter _ ->
            filter

        DetailsCommit filter _ ->
            filter

        DetailsView filter _ ->
            filter

        Selected1 filter _ ->
            filter

        Selected2 filter _ _ ->
            filter

        Search filter _ ->
            filter


type Msg
    = DoNothing
    | PushRoute Route
    | PushRouteAsString String
    | ChangeUrl String
    | ChangeLocalStorage String
    | ChangePwd String
    | ChangeDiffBlinkingSpeed Int
    | ToggleInfo
    | RemoveHttpError
    | Response (Result Http.Error (List Screenshot.Screenshot))
    | EmptyWarnings
    | MouseMove String MouseMoveData
    | MouseLeave


type Route
    = Top Filter.Filter
    | Latest Filter.Filter
    | History Filter.Filter
    | Selected1 Filter.Filter Screenshot.Screenshot
    | Selected2 Filter.Filter Screenshot.Screenshot Screenshot.Screenshot
    | NotFound Filter.Filter String
      --
    | DetailsImage Filter.Filter Image
    | DetailsCommit Filter.Filter Commit
    | DetailsView Filter.Filter View
    | Search Filter.Filter String


type alias MouseMoveData =
    { offsetX : Int
    , offsetY : Int
    , offsetHeight : Float
    , offsetWidth : Float
    }


type View
    = View String


type Image
    = Image String


type Commit
    = Commit String
