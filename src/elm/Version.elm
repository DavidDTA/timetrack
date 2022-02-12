module Version exposing (Version, equals, fromRaw, increment, toRaw, zero)


type Version
    = Version Int


zero =
    Version 0


increment (Version previous) =
    Version (previous + 1)


fromRaw raw =
    Version raw


toRaw (Version raw) =
    raw


equals (Version a) (Version b) =
    a == b
