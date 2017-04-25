module Scheme.Types exposing (..)

import Html


main : Html.Html msg
main =
    Html.text ""


type Tree a
    = Branch (List (Tree a))
    | Leaf a
