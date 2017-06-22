module Utils.Number exposing (..)

{-| A (de facto?) standard pretty formatting for numbers.
    pretty 2 ',' '.' 81601710123.338023  == "81,601,710,123.34"
    pretty 3 ' ' '.' 81601710123.338023  == "81 601 710 123.338"
    pretty 3 ' ' '.' -81601710123.338023 == "-81 601 710 123.34"
* Numbers are rounded to the nearest printable digit
* Digits before the decimal are grouped into spans of three and separated by a seperator character
-}

--- This is an implementation in ELm 0.18 of elm-number-format


pretty : Int -> Char -> Char -> Float -> String
pretty decimals sep ds n =
    let
        decpow =
            10 ^ decimals

        nshift =
            n * toFloat decpow

        nshifti =
            round nshift

        nshifti2 =
            abs nshifti

        ni =
            nshifti2 // decpow

        nf =
            nshifti2 - ni * decpow

        nfs =
            toString nf

        nflen =
            String.length nfs
    in
        String.append
            (if nshifti < 0 then
                prettyInt sep -ni
             else
                prettyInt sep ni
            )
            (String.cons
                ds
                (String.padLeft decimals '0' nfs)
            )


{-| A (de facto?) standard pretty formatting for numbers.
This version of the function operates on integers instead of floating point values.
In future `pretty` may be used on both integers as well as floating point values and this function
will be deprecated.
    prettyInt ',' 81601710123  == "81,601,710,123"
    prettyInt ' ' 81601710123  == "81 601 710 123"
    prettyInt ' ' -81601710123 == "-81 601 710 123"
* Digits are grouped into spans of three and separated by a seperator character
-}
prettyInt : Char -> Int -> String
prettyInt sep n =
    let
        ni =
            abs n

        nis =
            String.join (String.fromChar sep) (chunksOfRight 3 <| toString ni)
    in
        if n < 0 then
            String.cons '-' nis
        else
            nis


chunksOfRight : Int -> String -> List String
chunksOfRight k s =
    let
        len =
            String.length s

        k2 =
            2 * k

        chunksOfR s2 =
            if String.length s2 > k2 then
                String.right k s2 :: chunksOfR (String.dropRight k s2)
            else
                String.right k s2 :: [ String.dropRight k s2 ]
    in
        if len > k2 then
            List.reverse (chunksOfR s)
        else if len > k then
            String.dropRight k s :: [ String.right k s ]
        else
            [ s ]
