module ScreenshotsBy exposing
    ( byCategory
    , byId
    , byTime
    )

import Dict
import Screenshot
import Set
import SharedTypes exposing (..)


byTime :
    List Screenshot.Screenshot
    -> Dict.Dict String (List Screenshot.Screenshot)
byTime filteredScreenshots =
    filteredScreenshots
        |> List.foldl
            (\screenshot ->
                Dict.update screenshot.time (helperAddToAList screenshot)
            )
            Dict.empty


byId :
    List Screenshot.Screenshot
    -> Dict.Dict String (List Screenshot.Screenshot)
byId filteredScreenshots =
    filteredScreenshots
        |> List.foldl
            (\screenshot ->
                let
                    screenshotsIdAsString : String
                    screenshotsIdAsString =
                        screenshot
                            |> Screenshot.toScreenshotId
                            |> Screenshot.screenshotIdToString
                in
                Dict.update screenshotsIdAsString (helperAddToAList screenshot)
            )
            Dict.empty


emptyByCategory : ScreenshotsByCategory
emptyByCategory =
    { time = Dict.empty
    , size = Dict.empty
    , cat2 = Dict.empty
    , cat3 = Dict.empty
    , cat4 = Dict.empty
    , cat5 = Dict.empty
    , cat6 = Dict.empty
    , cat7 = Dict.empty
    , cat8 = Dict.empty
    , cat9 = Dict.empty
    , cat10 = Dict.empty
    }


byCategory : List Screenshot.Screenshot -> ScreenshotsByCategory
byCategory screenshots =
    List.foldl
        (\screenshot acc ->
            { time = Dict.update screenshot.time (helperAddToASet screenshot) acc.time
            , size = Dict.update (Screenshot.imageSizeToString screenshot.size) (helperAddToASet screenshot) acc.size
            , cat2 = Dict.update screenshot.cat2 (helperAddToASet screenshot) acc.cat2
            , cat3 = Dict.update screenshot.cat3 (helperAddToASet screenshot) acc.cat3
            , cat4 = Dict.update screenshot.cat4 (helperAddToASet screenshot) acc.cat4
            , cat5 = Dict.update screenshot.cat5 (helperAddToASet screenshot) acc.cat5
            , cat6 = Dict.update screenshot.cat6 (helperAddToASet screenshot) acc.cat6
            , cat7 = Dict.update screenshot.cat7 (helperAddToASet screenshot) acc.cat7
            , cat8 = Dict.update screenshot.cat8 (helperAddToASet screenshot) acc.cat8
            , cat9 = Dict.update screenshot.cat9 (helperAddToASet screenshot) acc.cat9
            , cat10 = Dict.update screenshot.cat10 (helperAddToASet screenshot) acc.cat10
            }
        )
        emptyByCategory
        screenshots


helperAddToASet : Screenshot.Screenshot -> Maybe (Set.Set String) -> Maybe (Set.Set String)
helperAddToASet screenshot maybeV =
    case maybeV of
        Nothing ->
            Just <| Set.fromList [ Screenshot.imageSrcToString <| Screenshot.toImageSrc screenshot ]

        Just v ->
            Just <| Set.insert (Screenshot.imageSrcToString <| Screenshot.toImageSrc screenshot) v


helperAddToAList : v -> Maybe (List v) -> Maybe (List v)
helperAddToAList newV maybeV =
    case maybeV of
        Nothing ->
            Just [ newV ]

        Just v ->
            Just <| newV :: v
