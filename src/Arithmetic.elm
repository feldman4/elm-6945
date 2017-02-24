module Arithmetic exposing (..)

-- would be nice to have two examples of type Arithmetic and a combiner...
-- can't translate the "install" procedure, need to go with static mechanism
-- makeBinaryOp: arithmetic name op a b
-- arithmetic decides how function application works
-- basically implements its own versions of makeNaryOp
-- predicate dispatch is allowed
-- how to union
-- could match on arguments and Maybe operate
-- makeBinaryOp tries arithmetics until one works, or returns a FuckIt
{-
   (define (make-arithmetic name
                            domain-predicate
                            bases
                            get-constant
                            get-operation)
     (guarantee predicate? domain-predicate)
     (guarantee-list-of arithmetic? bases)
     (%make-arithmetic
      (cons name (map arithmetic-name bases))
      bases
      domain-predicate
      ;; TODO(cph): Eliding these calls when the number of results
      ;; doesn't equal the number of bases is arbitrary and should
      ;; be reconsidered.
      (filter-map (lambda (name)
                    (let ((base-constants
                           (arithmetic-constants-for name bases)))
                      (and (n:= (length bases)
                                (length base-constants))
                           (cons name
                                 (apply get-constant
                                        name
                                        base-constants)))))
                  (arithmetic-constant-names-for bases))
      (filter-map (lambda (operator)
                    (let ((base-operations
                           (arithmetic-operations-for operator
                                                      bases)))
                      (and (n:= (length bases)
                                (length base-operations))
                           (cons operator
                                 (apply get-operation
                                        operator
                                        base-operations)))))
                  (arithmetic-operators-for bases))))

-}


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
