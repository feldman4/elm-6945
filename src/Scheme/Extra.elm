module Scheme.Extra exposing (..)

import Html exposing (text, div)
import Scheme exposing (..)
import Scheme.Types exposing (..)
import Scheme.View exposing (viewTree, drawNode, drawLabeledNode)
import Svg
import Maybe.Extra


main : Html.Html msg
main =
    text ""


drawNodeData : Data -> Svg.Svg msg
drawNodeData data =
    let
        label =
            case data of
                Boolean bool ->
                    bool |> toString

                Number n ->
                    n |> toString

                Symbol s ->
                    if s == "lambda" then
                        "λ"
                    else
                        s

                Procedure p ->
                    "proc."

                Primitive label p ->
                    label

                Undefined ->
                    "fuck you"
    in
        drawLabeledNode label


program : Expression
program =
    Branch
        ([ Leaf (Symbol "if")
         , Branch
            ([ Leaf (Symbol "if")
             , Leaf (Boolean True)
             , Leaf (Boolean False)
             , Leaf (Boolean True)
             ]
            )
         , Leaf (Number 3)
         , Leaf (Number 4)
         ]
        )


program4 : Expression
program4 =
    Branch
        ([ Leaf (Symbol "begin")
         , Branch
            ([ Leaf (Symbol "set!")
             , Leaf (Symbol "x")
             , Leaf (Number 7)
             ]
            )
         , Leaf (Symbol "x")
         ]
        )


program2 : Expression
program2 =
    Branch
        ([ Leaf (Symbol "begin")
         , Branch
            ([ Leaf (Symbol "set!")
             , Leaf (Symbol "x")
             , Leaf (Number 7)
             ]
            )
         , Branch
            ([ Leaf (Symbol "set!")
             , Leaf (Symbol "f")
             , Branch
                ([ Leaf (Symbol "lambda")
                 , Branch ([ Leaf (Symbol "a"), Leaf (Symbol "b") ])
                 , Branch ([ Leaf (Symbol "+"), Leaf (Symbol "a"), Leaf (Symbol "b") ])
                 ]
                )
             ]
            )
         , Branch
            ([ Leaf (Symbol "f")
             , Leaf (Number -111)
             , Leaf (Symbol "x")
             ]
            )
         ]
        )


program3 : Expression
program3 =
    Branch
        ([ Leaf (Symbol "begin")
         , program
         , program
         ]
        )


program5 : Expression
program5 =
    Branch
        ([ Leaf (Symbol "+")
         , Leaf (Number 3)
         , Leaf (Number 4)
         ]
        )


finalize :
    Environment
    -> Expression
    -> Partial ( Environment, Tree Data ) ( Environment, Data )
finalize env expr =
    Continue ( env, expr ) (\() -> evalOnce env expr)


{-|
need to "lift" continuation by wrapping it in another continuation
-}
evalAndSubstitute :
    (Tree a -> Expression)
    -> Partial ( Environment, Tree a ) ( Environment, a )
    -> Partial ( Environment, Tree Data ) ( Environment, Data )
evalAndSubstitute partialExpr partial =
    case partial of
        Continue ( a, b ) f ->
            Continue ( a, partialExpr b ) (\() -> f () |> evalAndSubstitute partialExpr)

        -- done evaluating, but we make it look like an if statement
        -- with a Leaf predicate so it goes around one more time
        Done ( a, b ) ->
            finalize a (partialExpr (Leaf b))


evalOnce : Environment -> Expression -> Partial ( Environment, Expression ) ( Environment, Data )
evalOnce environment expression =
    case expression of
        Leaf (Symbol s) ->
            envGet environment s
                |> Maybe.withDefault Undefined
                |> Leaf
                |> finalize environment

        Leaf x ->
            Done ( environment, x )

        Branch ((Leaf (Symbol "lambda")) :: (Branch parameters) :: body) ->
            makeLambda parameters body
                |> Maybe.map Procedure
                |> Maybe.withDefault Undefined
                |> (\x -> Done ( environment, x ))

        Branch ((Leaf (Symbol "set!")) :: (Leaf (Symbol name)) :: value :: []) ->
            -- TODO: continuations
            Done ( evalAssignment name value environment, Undefined )

        Branch ((Leaf (Symbol "define")) :: ((Branch ((Leaf (Symbol name)) :: parameters)) :: body)) ->
            Branch
                [ Leaf (Symbol "set!")
                , Leaf (Symbol name)
                , Branch (Leaf (Symbol "lambda") :: Branch parameters :: body)
                ]
                |> evalOnce environment

        Branch ((Leaf (Symbol "begin")) :: exprs) ->
            evalBeginOnce environment exprs

        Branch ((Leaf (Symbol "if")) :: predicate :: consequent :: alternative :: []) ->
            case predicate of
                -- predicate fully simplified, move on
                Leaf x ->
                    if truthy x then
                        evalOnce environment consequent
                    else
                        evalOnce environment alternative

                Branch _ ->
                    case evalOnce environment predicate of
                        -- predicate not fully simplified
                        Continue ( subEnv, subExpr ) continuation ->
                            let
                                partialExpr pred =
                                    Branch ((Leaf (Symbol "if")) :: pred :: consequent :: alternative :: [])
                            in
                                Continue ( subEnv, partialExpr subExpr ) (\() -> continuation () |> evalAndSubstitute partialExpr)

                        -- done evaluating, but we make it look like an if statement
                        -- with a Leaf predicate so it goes around one more time
                        Done ( subEnv, data ) ->
                            let
                                partialExpr =
                                    Branch ((Leaf (Symbol "if")) :: (Leaf data) :: consequent :: alternative :: [])
                            in
                                finalize subEnv partialExpr

        Branch (operator :: arguments) ->
            -- "pre-eval" the operator and arguments (could involve set!)
            -- returned Procedure contains
            case eval environment expression of
                ( Just data, env2 ) ->
                    finalize env2 (Leaf data)

                _ ->
                    let
                        _ =
                            ( operator, arguments ) |> Debug.log "apply failed"
                    in
                        Done ( environment, Undefined )

        _ ->
            Done ( environment, Undefined )


evalBeginOnce : Environment -> List Expression -> Partial ( Environment, Tree Data ) ( Environment, Data )
evalBeginOnce environment body =
    case body of
        [] ->
            Done ( environment, Undefined )

        (Leaf data) :: [] ->
            finalize environment (Leaf data)

        expr :: rest ->
            case evalOnce environment expr of
                -- predicate not fully simplified
                Continue ( subEnv, subExpr ) continuation ->
                    let
                        partialExpr a =
                            Branch ((Leaf (Symbol "begin")) :: a :: rest)
                    in
                        Continue ( subEnv, partialExpr subExpr ) (\() -> continuation () |> evalAndSubstitute partialExpr)

                -- done evaluating, but we make it look like a begin statement
                -- with a Leaf body so it goes around one more time
                Done ( subEnv, data ) ->
                    let
                        partialExpr =
                            case rest of
                                [] ->
                                    Branch ((Leaf (Symbol "begin")) :: (Leaf data) :: [])

                                _ ->
                                    Branch ((Leaf (Symbol "begin")) :: rest)
                    in
                        finalize subEnv partialExpr


equalOp :
    Environment
    -> a
    -> (List Data -> Bool)
    -> Symbol
    -> b
    -> List (Tree Data)
    -> Partial ( Environment, Tree Data ) ( Environment, Data )
equalOp environment expression equality opName zero args =
    -- prepare arguments
    let
        bad n =
            case n of
                Leaf (Procedure _) ->
                    True

                _ ->
                    False

        aValue n =
            case n of
                Leaf data ->
                    True

                _ ->
                    False

        getValue n =
            case n of
                Leaf data ->
                    Just data

                _ ->
                    Nothing

        total =
            args |> List.map getValue |> Maybe.Extra.combine |> Maybe.map equality
    in
        if List.any bad args |> Debug.log "badness" then
            Done ( environment, Undefined )
        else
            case List.partition aValue args |> Debug.log "partitioned" of
                ( done, [] ) ->
                    finalize environment (Leaf (Boolean (total |> Maybe.withDefault True)))

                ( done, next :: rest ) ->
                    let
                        partialExpr n =
                            Branch (((Leaf (Symbol opName)) :: done) ++ (n :: rest))
                    in
                        case evalOnce environment next of
                            -- predicate not fully simplified
                            Continue ( subEnv, subExpr ) continuation ->
                                Continue ( subEnv, partialExpr subExpr ) (\() -> continuation () |> evalAndSubstitute partialExpr)

                            -- done evaluating, but we make it look like an if statement
                            -- with a Leaf predicate so it goes around one more time
                            Done ( subEnv, data ) ->
                                finalize subEnv (partialExpr (Leaf data))


binOp :
    Environment
    -> a
    -> (Float -> Float -> Float)
    -> Symbol
    -> Float
    -> List (Tree Data)
    -> Partial ( Environment, Tree Data ) ( Environment, Data )
binOp environment expression op opName zero args =
    -- prepare arguments
    let
        bad n =
            case n of
                Leaf (Number x) ->
                    False

                Leaf _ ->
                    True

                _ ->
                    False

        aNumber n =
            case n of
                Leaf (Number x) ->
                    True

                _ ->
                    False

        getNum n =
            case n of
                Leaf (Number x) ->
                    Just x

                _ ->
                    Nothing

        total =
            args |> List.map getNum |> Maybe.Extra.combine |> Maybe.map (List.foldl op zero)
    in
        if args |> Debug.log "args" |> List.any bad |> Debug.log "badness" then
            Done ( environment, Undefined )
        else
            case List.partition aNumber args |> Debug.log "partitioned" of
                ( done, [] ) ->
                    finalize environment (Leaf (Number (total |> Maybe.withDefault zero)))

                ( done, next :: rest ) ->
                    let
                        partialExpr n =
                            Branch (((Leaf (Symbol opName)) :: done) ++ (n :: rest))
                    in
                        case evalOnce environment next of
                            -- predicate not fully simplified
                            Continue ( subEnv, subExpr ) continuation ->
                                Continue ( subEnv, partialExpr subExpr ) (\() -> continuation () |> evalAndSubstitute partialExpr)

                            -- done evaluating, but we make it look like an if statement
                            -- with a Leaf predicate so it goes around one more time
                            Done ( subEnv, data ) ->
                                finalize subEnv (partialExpr (Leaf data))


binOpSub :
    Environment
    -> a
    -> (Float -> Float -> Float)
    -> Symbol
    -> Float
    -> List (Tree Data)
    -> Partial ( Environment, Tree Data ) ( Environment, Data )
binOpSub environment expression op opName zero args =
    -- prepare arguments
    let
        bad n =
            case n of
                Leaf (Number x) ->
                    False

                Leaf _ ->
                    True

                _ ->
                    False

        aNumber n =
            case n of
                Leaf (Number x) ->
                    True

                _ ->
                    False

        getNum n =
            case n of
                Leaf (Number x) ->
                    Just x

                _ ->
                    Nothing

        doTheSub xs =
            case xs of
                [] ->
                    zero

                x :: rest ->
                    op x (List.sum rest)

        total =
            args |> List.map getNum |> Maybe.Extra.combine |> Maybe.map doTheSub
    in
        if List.any bad args |> Debug.log "badness" then
            Done ( environment, Undefined )
        else
            case List.partition aNumber args |> Debug.log "partitioned" of
                ( done, [] ) ->
                    finalize environment (Leaf (Number (total |> Maybe.withDefault zero)))

                ( done, next :: rest ) ->
                    let
                        partialExpr n =
                            Branch (((Leaf (Symbol opName)) :: done) ++ (n :: rest))
                    in
                        case evalOnce environment next of
                            -- predicate not fully simplified
                            Continue ( subEnv, subExpr ) continuation ->
                                Continue ( subEnv, partialExpr subExpr ) (\() -> continuation () |> evalAndSubstitute partialExpr)

                            -- done evaluating, but we make it look like an if statement
                            -- with a Leaf predicate so it goes around one more time
                            Done ( subEnv, data ) ->
                                finalize subEnv (partialExpr (Leaf data))


evalThrough : Environment -> Expression -> ( List ( Environment, Expression ), Data )
evalThrough environment expression =
    let
        initial =
            evalOnce environment expression

        append partial xs =
            case partial of
                Continue x f ->
                    append (f ()) (x :: xs)

                Done ( _, data ) ->
                    ( xs, data )
    in
        append initial [ ( environment, expression ) ]


type Partial a b
    = Continue a (() -> Partial a b)
    | Done b
