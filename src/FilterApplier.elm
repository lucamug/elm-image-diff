module FilterApplier exposing (filterScreenshots)

import Dict
import Filter
import Maybe.Extra
import Screenshot
import Set
import SharedTypes exposing (..)


filterScreenshots :
    Filter.Filter
    -> ScreenshotsByCategory
    -> List Screenshot.Screenshot
    -> List Screenshot.Screenshot
filterScreenshots filter screenshotsByCategory screenshots =
    if filter == Filter.empty then
        screenshots

    else
        screenshots
            -- Converting screenshots to strings
            |> List.map (Screenshot.toImageSrc >> Screenshot.imageSrcToString)
            -- Converting to a set, to make then intersectable
            |> Set.fromList
            |> filterCat filter screenshotsByCategory "size"
            |> filterCat filter screenshotsByCategory "cat2"
            |> filterCat filter screenshotsByCategory "cat3"
            |> filterCat filter screenshotsByCategory "cat4"
            |> filterCat filter screenshotsByCategory "cat5"
            |> filterCat filter screenshotsByCategory "cat6"
            |> filterCat filter screenshotsByCategory "cat7"
            |> filterCat filter screenshotsByCategory "cat8"
            |> filterCat filter screenshotsByCategory "cat9"
            |> filterCat filter screenshotsByCategory "cat10"
            |> filterCat filter screenshotsByCategory "time"
            |> Set.toList
            |> List.map Screenshot.fromString
            |> Maybe.Extra.values


filterCat : Filter.Filter -> ScreenshotsByCategory -> String -> Set.Set String -> Set.Set String
filterCat filter cached_allScreenshotsByCategory categoryKey partiallyFilteredScreenshots =
    let
        filterFiltered : Filter.Filter
        filterFiltered =
            Filter.filterFiltered categoryKey filter
    in
    if Filter.isEmpty filterFiltered then
        -- There are no filters set for this category so
        -- we let the entire set just go unmodified
        partiallyFilteredScreenshots

    else
        let
            screenshotAsUrlByCategoryAsString : Dict.Dict String (Set.Set String)
            screenshotAsUrlByCategoryAsString =
                case categoryKey of
                    "size" ->
                        .size cached_allScreenshotsByCategory

                    "cat2" ->
                        .cat2 cached_allScreenshotsByCategory

                    "cat3" ->
                        .cat3 cached_allScreenshotsByCategory

                    "cat4" ->
                        .cat4 cached_allScreenshotsByCategory

                    "cat5" ->
                        .cat5 cached_allScreenshotsByCategory

                    "cat6" ->
                        .cat6 cached_allScreenshotsByCategory

                    "cat7" ->
                        .cat7 cached_allScreenshotsByCategory

                    "cat8" ->
                        .cat8 cached_allScreenshotsByCategory

                    "cat9" ->
                        .cat9 cached_allScreenshotsByCategory

                    "cat10" ->
                        .cat10 cached_allScreenshotsByCategory

                    "time" ->
                        .time cached_allScreenshotsByCategory

                    _ ->
                        Dict.empty
        in
        if List.length (Dict.toList screenshotAsUrlByCategoryAsString) == Filter.length filterFiltered then
            -- All possibility are unselected, so returning
            -- an empty set
            Set.empty

        else
            Filter.incrementalFilter
                { filter = filterFiltered
                , categoryKey = categoryKey
                , screenshotByCategory = screenshotAsUrlByCategoryAsString
                , partiallyFilteredScreenshots = partiallyFilteredScreenshots
                }
