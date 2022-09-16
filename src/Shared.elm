module Shared exposing (..)

--
-- THIS MODULE SHOULD ONLY HAVE DEPENDENCIES FROM THIRD PARTIES MODULES
--

import DateFormat
import Time


itemsPerPage : Int
itemsPerPage =
    70


addThousandSeparator : Int -> String
addThousandSeparator int =
    int
        |> String.fromInt
        |> String.split ""
        |> List.reverse
        |> splitEvery 3
        |> intercalate [ "," ]
        |> List.reverse
        |> String.join ""


splitEvery : Int -> List a -> List (List a)
splitEvery n l =
    let
        loop : Int -> List a -> List (List a) -> List (List a)
        loop n_ rem acc =
            case rem of
                [] ->
                    List.reverse acc

                _ ->
                    loop n_ (List.drop n_ rem) (List.take n_ rem :: acc)
    in
    loop n l []


intercalate : List a -> List (List a) -> List a
intercalate list listOfList =
    List.concat (List.intersperse list listOfList)


helperAddToAList : v -> Maybe (List v) -> Maybe (List v)
helperAddToAList newV maybeV =
    case maybeV of
        Nothing ->
            Just [ newV ]

        Just v ->
            Just <| newV :: v


calculateWidth : String -> Int -> Int
calculateWidth screenSize height =
    case String.toLower screenSize of
        "mobile" ->
            -- height : 1000 = x : 320
            height * screenSizeMobile.x // screenSizeMobile.y

        _ ->
            -- height : 1200 = x : 840
            height * screenSizeDesktop.x // screenSizeDesktop.y


screenSizeMobile : { x : number, y : number1 }
screenSizeMobile =
    { x = 320, y = 1000 }


screenSizeDesktop : { x : number, y : number1 }
screenSizeDesktop =
    { x = 840, y = 1200 }


maxImagesPerRow : number
maxImagesPerRow =
    10


dateFormatterShort : Time.Zone -> Time.Posix -> String
dateFormatterShort =
    DateFormat.format
        [ DateFormat.monthFixed
        , DateFormat.text "."
        , DateFormat.dayOfMonthFixed
        ]


dateFormatter : Time.Zone -> Time.Posix -> String
dateFormatter =
    DateFormat.format
        [ DateFormat.monthNameFull
        , DateFormat.text " "
        , DateFormat.dayOfMonthSuffix
        , DateFormat.text " - "
        , DateFormat.hourFixed
        , DateFormat.text ":"
        , DateFormat.minuteFixed
        , DateFormat.text ":"
        , DateFormat.secondFixed
        ]
