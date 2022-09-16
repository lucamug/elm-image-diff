module Filter exposing
    ( Category
    , Filter
    , allFiltersForThisCategory
    , diff
    , empty
    , filterFiltered
    , fromMaybeString
    , incrementalFilter
    , isEmpty
    , length
    , memberCategory
    , newFilter
    , toMaybeString
    , union
    )

import Dict
import Maybe.Extra
import Set


type Filter
    = Filter (Set.Set String)


categoryKeyValueSeparator : String
categoryKeyValueSeparator =
    "__"


categorySeparator : String
categorySeparator =
    "~"


fromMaybeString : Maybe String -> Filter
fromMaybeString maybeString =
    maybeString
        |> Maybe.map (String.split categorySeparator)
        |> Maybe.withDefault []
        |> Set.fromList
        |> Filter


toMaybeString : Filter -> Maybe String
toMaybeString (Filter set1) =
    set1
        |> Set.toList
        |> String.join categorySeparator
        |> (\string ->
                if string == "" then
                    Nothing

                else
                    Just string
           )


empty : Filter
empty =
    Filter Set.empty


diff : Filter -> Filter -> Filter
diff (Filter set1) (Filter set2) =
    Set.diff set1 set2
        |> Filter


union : Filter -> Filter -> Filter
union (Filter set1) (Filter set2) =
    Set.union set1 set2
        |> Filter


filter : (String -> Bool) -> Filter -> Filter
filter f (Filter set1) =
    set1
        |> Set.filter f
        |> Filter


member : String -> Filter -> Bool
member string (Filter set1) =
    Set.member string set1


isEmpty : Filter -> Bool
isEmpty (Filter set1) =
    Set.isEmpty set1


length : Filter -> Int
length (Filter set1) =
    set1
        |> Set.toList
        |> List.length


remove : String -> Filter -> Filter
remove categoryKeyValue (Filter set1) =
    set1
        |> Set.remove categoryKeyValue
        |> Filter


insert : String -> Filter -> Filter
insert categoryKeyValue (Filter set1) =
    set1
        |> Set.insert categoryKeyValue
        |> Filter


toSet : Filter -> Set.Set String
toSet (Filter set1) =
    set1



--
-- CATEGORY STUFF
--


type alias Category =
    { categoryKey : String, categoryValue : String }


newFilter : { categoryKey : String, categoryValue : String } -> Filter -> Filter
newFilter category filter_ =
    let
        member_ : Bool
        member_ =
            memberCategory
                { categoryKey = category.categoryKey
                , categoryValue = category.categoryValue
                }
                filter_
    in
    if member_ then
        removeCategory category filter_

    else
        insertCategory category filter_


filterFiltered : String -> Filter -> Filter
filterFiltered categoryKey filter_ =
    filter_
        |> filter (\f -> String.startsWith (categoryKey ++ categoryKeyValueSeparator) f)


incrementalFilter :
    { filter : Filter
    , categoryKey : String
    , screenshotByCategory : Dict.Dict String (Set.Set String)
    , partiallyFilteredScreenshots : Set.Set String
    }
    -> Set.Set String
incrementalFilter args =
    args.filter
        |> toSet
        |> Set.map (String.replace (args.categoryKey ++ categoryKeyValueSeparator) "")
        |> Set.toList
        |> List.map (\f -> Dict.get f args.screenshotByCategory)
        |> Maybe.Extra.values
        |> List.foldl (\list acc -> Set.diff acc list) args.partiallyFilteredScreenshots


allFiltersForThisCategory : (a -> Dict.Dict String (Set.Set String)) -> String -> a -> Filter
allFiltersForThisCategory categorySelector categoryKey cached_allScreenshotsByCategory =
    Dict.keys (categorySelector cached_allScreenshotsByCategory)
        |> List.map
            (\categoryValue ->
                categoryToString
                    { categoryKey = categoryKey
                    , categoryValue = categoryValue
                    }
            )
        |> Set.fromList
        |> Filter


memberCategory : Category -> Filter -> Bool
memberCategory category filter_ =
    member (categoryToString category) filter_


removeCategory : Category -> Filter -> Filter
removeCategory category filter_ =
    remove (categoryToString category) filter_


insertCategory : Category -> Filter -> Filter
insertCategory category filter_ =
    insert (categoryToString category) filter_


categoryToString : Category -> String
categoryToString category =
    String.join categoryKeyValueSeparator [ category.categoryKey, category.categoryValue ]
