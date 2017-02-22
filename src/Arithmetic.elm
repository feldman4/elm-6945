module Arithmetic exposing (..)


type Quantity
    = Num Number
    | Sym Name


type alias UnaryOp =
    Quantity -> Quantity


type alias BinaryOp =
    Quantity -> Quantity -> Quantity


type alias Name =
    String


type alias Number =
    Float


listify : List String -> String
listify xs =
    "(" ++ (xs |> String.join " ") ++ ")"


makeUnaryOp : Name -> (Number -> Number) -> UnaryOp
makeUnaryOp name op a =
    let
        listify1 x =
            listify [ name, x ]
    in
        case a of
            Num a_ ->
                Num (op a_)

            Sym a_ ->
                Sym (listify1 a_)


makeBinaryOp : String -> (Number -> Number -> Number) -> BinaryOp
makeBinaryOp name op a b =
    let
        listify2 x y =
            listify [ name, x, y ]
    in
        case ( a, b ) of
            ( Num a_, Num b_ ) ->
                Num (op a_ b_)

            ( Sym a_, Sym b_ ) ->
                Sym (listify2 a_ b_)

            ( Num a_, Sym b_ ) ->
                Sym (listify2 (a_ |> toString) b_)

            ( Sym a_, Num b_ ) ->
                Sym (listify2 a_ (b_ |> toString))


(+.) : BinaryOp
(+.) =
    makeBinaryOp "+" (+)


(-.) : BinaryOp
(-.) =
    makeBinaryOp "-" (-)


(*.) : BinaryOp
(*.) =
    makeBinaryOp "*" (*)


(/.) : BinaryOp
(/.) =
    makeBinaryOp "/" (/)


infixr 4 *.


infixr 3 /.


infixr 2 +.


infixr 1 -.
