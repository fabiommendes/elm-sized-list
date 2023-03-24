module SizedList exposing
    ( SizedList
    , cons, singleton, repeat, fromList
    , map, indexedMap, foldl, foldr
    , filter, filterMap
    , append, concat, concatMap, map2, map3, map4, map5
    , sort, sortBy, sortWith
    , isEmpty, head, tail, take, drop, partition, uncons, unzip
    , length, all, any, minimum, maximum, sum, product, member, reverse, toList
    , empty, getAt, range, removeAt, updateAt
    )

{-| A list with at least one element

@docs SizedList


## Construction

@docs create, cons, singleton, repeat, fromList, withDefault, withExample


## Transform

@docs map, indexedMap, foldl, foldr, reduce


## Filtering

@docs filter, filterMap


## Combine

@docs append, concat, concatMap, intersperse, map2, map3, map4, map5


## Sort

@docs sort, sortBy, sortWith


## Deconstruct

@docs isEmpty, head, tail, take, drop, partition, uncons, unzip


## Utilities

@docs length, all, any, minimum, maximum, sum, product, member, reverse, toList

-}

import List.Extra as List


{-| A list of a\`s with at least 2 elements.

Meta information can be associated to each segment pair.

-}
type SizedList a
    = SizedList Int (List a)



--- Create


{-| Create a list with only one element
-}
empty : SizedList a
empty =
    SizedList 0 []


{-| Create a list with only one element
-}
singleton : a -> SizedList a
singleton x =
    SizedList 1 [ x ]


{-| Create a list with n (at least 2) copies of a value
-}
repeat : Int -> a -> SizedList a
repeat n x =
    SizedList n (List.repeat n x)


{-| Create list from a range of values.

The first value is guaranteed to appear in the list.

-}
range : Int -> Int -> SizedList Int
range a b =
    SizedList (max 0 (b - a)) (List.range a b)


{-| Try to create sequence from a list
-}
fromList : List a -> SizedList a
fromList xs =
    SizedList (List.length xs) xs


{-| Add an element to the front of a list.
-}
cons : a -> SizedList a -> SizedList a
cons x (SizedList n xs) =
    SizedList (n + 1) (x :: xs)


{-| Convert data to list
-}
toList : SizedList a -> List a
toList (SizedList _ xs) =
    xs


{-| Apply a function to every element of a list.
-}
map : (a -> b) -> SizedList a -> SizedList b
map f (SizedList n xs) =
    SizedList n (List.map f xs)


{-| Apply a two argument function pairwise to elements of both lists
-}
map2 : (a -> b -> r) -> SizedList a -> SizedList b -> SizedList r
map2 f (SizedList n xs) (SizedList m ys) =
    SizedList (min n m) (List.map2 f xs ys)


{-| Apply a 3 argument function tuples of elements from all lists
-}
map3 : (a -> b -> c -> r) -> SizedList a -> SizedList b -> SizedList c -> SizedList r
map3 f (SizedList n xs) (SizedList m ys) (SizedList k zs) =
    SizedList (min m (min n k)) (List.map3 f xs ys zs)


{-| Apply a 4 argument function tuples of elements from all lists
-}
map4 : (a -> b -> c -> d -> r) -> SizedList a -> SizedList b -> SizedList c -> SizedList d -> SizedList r
map4 f (SizedList n xs) (SizedList m ys) (SizedList k zs) (SizedList i ws) =
    SizedList (min m (min n (min k i))) (List.map4 f xs ys zs ws)


{-| Apply a 5 argument function tuples of elements from all lists
-}
map5 : (a -> b -> c -> d -> e -> r) -> SizedList a -> SizedList b -> SizedList c -> SizedList d -> SizedList e -> SizedList r
map5 f (SizedList n xs) (SizedList m ys) (SizedList k zs) (SizedList i ws) (SizedList j rs) =
    SizedList (min m (min n (min k (min i j)))) (List.map5 f xs ys zs ws rs)


{-| Sort values from lowest to highest
-}
sort : SizedList comparable -> SizedList comparable
sort (SizedList n xs) =
    SizedList n (List.sort xs)


{-| Sort values by a derived property.
-}
sortBy : (a -> comparable) -> SizedList a -> SizedList a
sortBy f (SizedList n xs) =
    SizedList n (List.sortBy f xs)


{-| Sort values with a custom comparison function.

This is also the most general sort function, allowing you to define any other: sort == sortWith compare

-}
sortWith : (a -> a -> Order) -> SizedList a -> SizedList a
sortWith f (SizedList n xs) =
    SizedList n (List.sortWith f xs)


{-| Same as map but the function is also applied to the index of each element (starting at zero).
-}
indexedMap : (Int -> a -> b) -> SizedList a -> SizedList b
indexedMap f (SizedList n xs) =
    SizedList n (List.indexedMap f xs)


{-| Reduce a list from the left.
-}
foldl : (a -> b -> b) -> b -> SizedList a -> b
foldl f acc (SizedList _ xs) =
    List.foldl f acc xs


{-| Reduce a list from the right.
-}
foldr : (a -> b -> b) -> b -> SizedList a -> b
foldr f acc (SizedList _ xs) =
    List.foldr f acc xs


{-| Keep elements that satisfy the test.

Return a list, since result might be empty.

-}
filter : (a -> Bool) -> SizedList a -> SizedList a
filter pred list =
    foldr
        (\x xs ->
            if pred x then
                cons x xs

            else
                xs
        )
        empty
        list


{-| Filter out certain values.

Return a list, since result might be empty.

-}
filterMap : (a -> Maybe b) -> SizedList a -> SizedList b
filterMap f =
    let
        maybeCons mx xs =
            case f mx of
                Just x ->
                    cons x xs

                Nothing ->
                    xs
    in
    foldr maybeCons empty


{-| Determine the length of a list.

    length (range 1 5) == 5

-}
length : SizedList a -> Int
length (SizedList n _) =
    n


{-| Reverse a list.

    reverse (range 1 3)
        |> toList
        == [ 5, 4, 3, 2, 1 ]

-}
reverse : SizedList a -> SizedList a
reverse (SizedList n xs) =
    SizedList n (List.reverse xs)


{-| Figure out whether a list contains a value.

    member 42 (range 1 3) == False

-}
member : a -> SizedList a -> Bool
member elem (SizedList _ xs) =
    List.member elem xs


{-| Put two lists together.

    append (range 1 3) (range 4 5) == range 1 5

-}
append : SizedList a -> SizedList a -> SizedList a
append (SizedList n xs) (SizedList m ys) =
    SizedList (n + m) (xs ++ ys)


{-| Concatenate a bunch of lists into a single list
-}
concat : SizedList (SizedList a) -> SizedList a
concat lists =
    foldr append empty lists


{-| Map a given function onto a list and flatten the resulting lists.

    concatMap f xs == concat (map f xs)

-}
concatMap : (a -> SizedList b) -> SizedList a -> SizedList b
concatMap f list =
    concat (map f list)


{-| Determine if all elements satisfy some test.

    all (\x -> modBy 2 == 0) (range 1 5) == False

-}
all : (a -> Bool) -> SizedList a -> Bool
all f (SizedList _ xs) =
    List.all f xs


{-| Determine if any elements satisfy some test.

    any (\x -> modBy 2 == 0) (range 1 5) == True

-}
any : (a -> Bool) -> SizedList a -> Bool
any f (SizedList _ xs) =
    List.any f xs


{-| Find the maximum element in a non-empty list.

    maximum (range 1 5) == 5

-}
maximum : SizedList comparable -> Maybe comparable
maximum (SizedList _ xs) =
    List.minimum xs


{-| Find the minimum element in a non-empty list.

    minimum (range 1 5) == 5

-}
minimum : SizedList comparable -> Maybe comparable
minimum (SizedList _ xs) =
    List.minimum xs


{-| Get the sum of the list elements.

    sum (range 1 5) == 1 + 2 + 3 + 4 + 5

-}
sum : SizedList number -> number
sum =
    foldl (+) 0


{-| Always return False, since SizedList is never empty :)
-}
isEmpty : SizedList a -> Bool
isEmpty (SizedList n _) =
    n == 0


{-| Get the product of the list elements.
-}
product : SizedList number -> number
product =
    foldl (+) 1


{-| Return the first element

    head (range 1 5) == 1

-}
head : SizedList a -> Maybe a
head (SizedList _ xs) =
    List.head xs


{-| Extract the rest of the list.

    head (range 1 5) == [ 2, 3, 4, 5 ]

-}
tail : SizedList a -> Maybe (SizedList a)
tail (SizedList n xs) =
    List.tail xs
        |> Maybe.map (SizedList (max (n - 1) 0))


{-| Drop (at most) the first n elements.

The resulting list will have at least one element

    drop 2 (range 1 5)
        |> toList
        == [ 3, 4, 5 ]

-}
drop : Int -> SizedList a -> SizedList a
drop n (SizedList m xs) =
    SizedList (max (m - n) 0) (List.drop n xs)


{-| Take (at most) the first n elements.

The number of elements n must be n >= 1, otherwise it returns the singleton list.

    take 2 (range 1 5)
        |> toList
        == [ 1, 2 ]

WARNING: differently from lists, `append (take n lst) (drop n lst)` do not always recover
the original list.

-}
take : Int -> SizedList a -> SizedList a
take n (SizedList m xs) =
    SizedList (min m n) (List.take n xs)


{-| Partition a list based on some test.

The first list contains all values that satisfy the test, and the second list contains all the value that do not.

-}
partition : (a -> Bool) -> SizedList a -> ( SizedList a, SizedList a )
partition pred (SizedList n xs) =
    let
        ( oks, fails ) =
            List.partition pred xs
                |> Tuple.mapFirst fromList
    in
    ( oks, SizedList (n - length oks) fails )


{-| Decompose a list of tuples into a tuple of lists.

    unzip (create ( "a", 1 ) [ ( "b", 2 ), ( "c", 3 ) ])
        == ( SizedList "a" [ "b", "c" ], SizedList 1 [ 2, 3 ] )

-}
unzip : SizedList ( a, b ) -> ( SizedList a, SizedList b )
unzip (SizedList n pairs) =
    let
        ( xs, ys ) =
            List.unzip pairs
    in
    ( SizedList n xs, SizedList n ys )


{-| Deconstruct list into the head and tail parts

    uncons (range 1 5)
        == ( 1, [ 2, 3, 4, 5 ] )

-}
uncons : SizedList a -> Maybe ( a, SizedList a )
uncons (SizedList n xs) =
    case xs of
        x :: rest ->
            Just ( x, SizedList (n - 1) rest )

        [] ->
            Nothing


{-| Update element at the given index

    updateAt 2 negate (range 1 5)
        |> toList
        == [ 1, 2, -3, 4, 5 ]

-}
updateAt : Int -> (a -> a) -> SizedList a -> SizedList a
updateAt i f (SizedList n xs) =
    SizedList n (List.updateAt i f xs)


{-| Remove the element at an index from a list.

Return the original list if the index is out of range or if the resulting list would be empty.

    removeAt 2 negate (range 1 5)
        |> toList
        == [ 1, 2, 4, 5 ]

-}
removeAt : Int -> SizedList a -> SizedList a
removeAt i (SizedList n xs) =
    SizedList n (List.removeAt i xs)


{-| Returns Just the element at the given index in the list, or Nothing if the index is out of range.

    getAt 1 (range 1 5) == Just 2

-}
getAt : Int -> SizedList a -> Maybe a
getAt i (SizedList _ xs) =
    List.getAt i xs
