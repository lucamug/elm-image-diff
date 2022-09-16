module Screenshot exposing
    ( Id
    , ImageSize
    , ImageSrc
    , Screenshot
    , empty
    , filterOnlySameIgnoringTime
    , fromString
    , idToString
    , imageSizeToString
    , imageSrcToString
    , screenshotIdToString
    , toId
    , toImageSrc
    , toScreenshotId
    )


type alias Screenshot =
    -- 2021-05-20.12.45.05_commit-abcd1234/desktop_light_uk_en-us_001a.png
    { time : String
    , differentPixels : Int
    , size : ImageSize
    , cat2 : String
    , cat3 : String
    , cat4 : String
    , cat5 : String
    , cat6 : String
    , cat7 : String
    , cat8 : String
    , cat9 : String
    , cat10 : String
    }


empty : Screenshot
empty =
    { time = ""
    , differentPixels = 0
    , size = { x = 0, y = 0 }
    , cat2 = ""
    , cat3 = ""
    , cat4 = ""
    , cat5 = ""
    , cat6 = ""
    , cat7 = ""
    , cat8 = ""
    , cat9 = ""
    , cat10 = ""
    }


type Id
    = Id String


type ImageSrc
    = ImageSrc String


toScreenshotId : Screenshot -> Id
toScreenshotId screenshot =
    [ imageSizeToString screenshot.size
    , screenshot.cat2
    , screenshot.cat3
    , screenshot.cat4
    , screenshot.cat5
    , screenshot.cat6
    , screenshot.cat7
    , screenshot.cat8
    , screenshot.cat9
    , screenshot.cat10
    ]
        |> String.join "_"
        |> Id


screenshotIdToString : Id -> String
screenshotIdToString (Id string) =
    string


screenshotEqualityIgnoringTime : Screenshot -> Screenshot -> Bool
screenshotEqualityIgnoringTime s1 s2 =
    { s1 | time = "", differentPixels = 0 } == { s2 | time = "", differentPixels = 0 }


filterOnlySameIgnoringTime : Maybe Screenshot -> List Screenshot -> List Screenshot
filterOnlySameIgnoringTime maybeScreenshotSelected screenshots =
    case maybeScreenshotSelected of
        Just screenshotSelected ->
            List.filter
                (screenshotEqualityIgnoringTime screenshotSelected)
                screenshots

        Nothing ->
            screenshots


imageSrcToString : ImageSrc -> String
imageSrcToString (ImageSrc string) =
    string


idToString : Id -> String
idToString (Id string) =
    string


toImageSrc : Screenshot -> ImageSrc
toImageSrc screenshot =
    [ String.join "/"
        [ ""
        , "screenshots"
        , screenshot.time
        , imageSizeToString screenshot.size
        ]
    , screenshot.cat2
    , screenshot.cat3
    , screenshot.cat4
    , screenshot.cat5
    , screenshot.cat6
    , screenshot.cat7
    , screenshot.cat8
    , screenshot.cat9
    , screenshot.cat10
    , String.fromInt screenshot.differentPixels
    ]
        |> String.join "_"
        |> (\string -> string ++ ".png")
        |> ImageSrc


toId : Screenshot -> Id
toId screenshot =
    [ screenshot.time
    , imageSizeToString screenshot.size
    , screenshot.cat2
    , screenshot.cat3
    , screenshot.cat4
    , screenshot.cat5
    , screenshot.cat6
    , screenshot.cat7
    , screenshot.cat8
    , screenshot.cat9
    , screenshot.cat10
    , String.fromInt screenshot.differentPixels
    ]
        |> String.join "_"
        |> Id


type alias ImageSize =
    { x : Int, y : Int }


imageSizeToString : ImageSize -> String
imageSizeToString { x, y } =
    String.fromInt x ++ "x" ++ String.fromInt y


stringToImageSize : String -> Maybe ImageSize
stringToImageSize string =
    case String.split "x" string of
        mx :: my :: _ ->
            case ( String.toInt mx, String.toInt my ) of
                ( Just x, Just y ) ->
                    Just
                        { x = x
                        , y = y
                        }

                _ ->
                    Nothing

        _ ->
            Nothing


fromString : String -> Maybe Screenshot
fromString screenshotAsUrl =
    let
        folderSeparator : String
        folderSeparator =
            "/"

        folderSeparator2 : String
        folderSeparator2 =
            "\\"

        secondarySeparator : String
        secondarySeparator =
            "_"

        fileExtension : String
        fileExtension =
            ".png"
    in
    screenshotAsUrl
        |> String.replace folderSeparator secondarySeparator
        |> String.replace folderSeparator2 secondarySeparator
        |> String.replace fileExtension ""
        |> String.split secondarySeparator
        |> List.reverse
        |> (\item ->
                case item of
                    differentPixels :: cat10 :: cat9 :: cat8 :: cat7 :: cat6 :: cat5 :: cat4 :: cat3 :: cat2 :: cat1 :: time2 :: time1 :: _ ->
                        case stringToImageSize cat1 of
                            Just xy ->
                                Just
                                    { time = String.join secondarySeparator [ time1, time2 ]
                                    , size = xy
                                    , cat2 = cat2
                                    , cat3 = cat3
                                    , cat4 = cat4
                                    , cat5 = cat5
                                    , cat6 = cat6
                                    , cat7 = cat7
                                    , cat8 = cat8
                                    , cat9 = cat9
                                    , cat10 = cat10
                                    , differentPixels = Maybe.withDefault 0 <| String.toInt differentPixels
                                    }

                            _ ->
                                Nothing

                    _ ->
                        Nothing
           )
