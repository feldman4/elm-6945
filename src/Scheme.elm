module Scheme exposing (..)

import Html exposing (text, div, textarea)
import Html.Attributes exposing (height, width, rows, cols)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import Maybe.Extra
import Scheme.Types exposing (..)


-- SPECIFICATION


truthy : Data -> Bool
truthy data =
    case data of
        Boolean False ->
            False

        Number 0 ->
            False

        _ ->
            True



--MORE TYPES
--EVAL


{-| (Maybe Value, Environment) is not as nice as Maybe (Value, Environment).
But it does let you signal that things like set! don't return a defined value,
without introducing Data = ... | Undefined.

Does not distinguish errors from undefined. Could upgrade to a Result type with
error messages.
-}
eval : Environment -> Expression -> ( Maybe Value, Environment )
eval environment expression =
    case expression of
        --to add lazy-memo:
        --  0. Data = ... | Thunk {ThunkStyle, Expression, Environment}
        --    enclosing environment can memoize evaluated Thunks. potentially fast
        --    since the substituted Thunks should be reference equal in Elm
        --    (benefit of s-expression instead of function)
        --  1. new syntax symbol "lazy-memo"
        --  2. in makeLambda, give procedure List (Operand, ThunkStyle)
        --  3. in apply, change substitutions to create Thunk depending on ThunkStyle
        --  4. overeager Thunk: force Thunk evaluation in certain places:
        --    a. primitive apply
        --    b. operator apply
        --    c. if predicate apply
        --    d. repl
        Leaf (Boolean x) ->
            ( Just (Boolean x), environment )

        Leaf (Number x) ->
            ( Just (Number x), environment )

        Leaf (Symbol s) ->
            ( envGet environment s, environment )

        Leaf (Primitive p) ->
            ( Just (Primitive p), environment )

        Branch ((Leaf (Symbol "define")) :: ((Branch ((Leaf (Symbol name)) :: parameters)) :: body)) ->
            let
                function =
                    makeLambda parameters body
            in
                case function of
                    Just proc ->
                        ( Nothing, evalAssignment name (Leaf (Procedure proc)) environment )

                    Nothing ->
                        ( Nothing, environment )

        Branch ((Leaf (Symbol "lambda")) :: (Branch parameters) :: body) ->
            -- MIT Scheme requires lambda parameters to be valid identifiers
            -- (aka Symbol?)
            case makeLambda parameters body of
                Just procedure ->
                    ( Just (Procedure procedure), environment )

                Nothing ->
                    ( Nothing, environment )

        Branch ((Leaf (Symbol "begin")) :: exprs) ->
            evalBegin exprs environment

        Branch ((Leaf (Symbol "debug")) :: expr :: []) ->
            let
                ( value, env2 ) =
                    eval environment expr

                _ =
                    expr
                        |> Debug.log "debug expression"
                        |> (\_ -> Debug.log "debug value" value)
            in
                ( value, env2 )

        Branch ((Leaf (Symbol "if")) :: predicate :: consequent :: alternative :: []) ->
            case eval environment predicate of
                -- ugh. set! is permitted everywhere.
                ( Just data, env2 ) ->
                    if truthy data then
                        eval env2 consequent
                    else
                        eval env2 alternative

                ( _, env2 ) ->
                    ( Nothing, environment )

        Branch ((Leaf (Symbol "set!")) :: (Leaf (Symbol name)) :: value :: []) ->
            ( Nothing, evalAssignment name value environment )

        Branch (operator :: arguments) ->
            evalProcedure environment operator arguments

        _ ->
            ( Nothing, environment )


{-| MIT Scheme requires lambda parameters to be valid identifiers
-}
makeLambda : List Expression -> List Expression -> Maybe Procedure
makeLambda parameters body =
    let
        toIdentifier data =
            case data of
                Leaf (Symbol s) ->
                    Just s

                _ ->
                    Nothing
    in
        parameters
            |> List.map toIdentifier
            |> Maybe.Extra.combine
            |> Maybe.map (\ps -> ( ps, Branch (Leaf (Symbol "begin") :: body) ))


{-| Not not implemented. Requires threading Environment through eval.
(set! (if #t x y) 3) throws "Variable required in this context" error in MIT
Scheme. So, we're not supporting expressions that simplify to the variable to
bind, either.
-}
evalAssignment : Symbol -> Expression -> Environment -> Environment
evalAssignment name value environment =
    case eval environment value of
        ( Just data, env2 ) ->
            envUpdate env2 name data

        _ ->
            environment


evalProcedure : Environment -> Expression -> List Expression -> ( Maybe Data, Environment )
evalProcedure environment operator arguments =
    let
        evalArgs env args =
            evalArgsApplicative env args
                |> Debug.log "eval'd args"
                |> Maybe.map (\( xs, env2 ) -> ( xs, env2 ))
    in
        -- eval the operator, then the arguments
        case eval environment operator of
            ( Just (Procedure p), env2 ) ->
                case evalArgs env2 arguments of
                    Just ( argumentData, env3 ) ->
                        eval env3 <| apply p argumentData

                    Nothing ->
                        ( Nothing, environment )

            ( Just (Primitive p), env2 ) ->
                case evalArgs env2 arguments of
                    Just ( argumentData, env3 ) ->
                        p argumentData
                            |> (\x -> ( x, env3 ))

                    Nothing ->
                        ( Nothing, environment )

            _ ->
                ( Nothing, environment )


evalArgsApplicative :
    Environment
    -> List Expression
    -> Maybe ( List Data, Environment )
evalArgsApplicative environment arguments =
    let
        -- combine each (Maybe Data, Environment) to get Maybe (List Data, Environment)
        -- could rewrite this fold with Maybe.map
        f : Expression -> Maybe ( List Data, Environment ) -> Maybe ( List Data, Environment )
        f expr soFar =
            case soFar of
                Just ( args, env ) ->
                    case eval env expr of
                        ( Just data, env2 ) ->
                            Just ( args ++ [ data ], env2 )

                        _ ->
                            Nothing

                _ ->
                    Nothing
    in
        List.foldl f (Just ( [], environment )) arguments


evalBegin : List Expression -> Environment -> ( Maybe Value, Environment )
evalBegin expressions environment =
    case expressions of
        [] ->
            ( Nothing, environment )

        expression :: [] ->
            eval environment expression

        expression :: rest ->
            eval environment expression |> (\( _, env2 ) -> evalBegin rest env2)



-- APPLY


{-| Currently substitutes values for symbols in the body. The other way is to
update the variable binding in the environment. Need the second way to build a
closure.

Parsing the code gave us a tree. The (tree, environment) pair is subjected to
the two maps eval and apply. Here there's a choice in whether to map the
-}
apply : Procedure -> List Value -> Expression
apply ( operands, body ) arguments =
    let
        substitutions =
            Dict.fromList (List.map2 (,) operands arguments)

        bind data =
            case data of
                Symbol s ->
                    Dict.get s substitutions |> Maybe.withDefault (Symbol s)

                _ ->
                    data
    in
        mapTree bind body


{-| could combine name recognition and execution in type
String -> List Data -> Data
-}
primitiveProcedure : String -> Maybe Primitive
primitiveProcedure name =
    case name of
        "+" ->
            Just (addlikeOp (+) 0)

        "*" ->
            Just (addlikeOp (*) 1)

        "=" ->
            Just (Just << listEqual)

        _ ->
            Nothing


listEqual : List Data -> Data
listEqual xs =
    case xs of
        [] ->
            Boolean True

        x :: rest ->
            rest |> List.all (equal x) |> Boolean


equal : Data -> Data -> Bool
equal a b =
    case ( a, b ) of
        ( Number x, Number y ) ->
            x == y

        ( Boolean x, Boolean y ) ->
            x == y

        ( Primitive x, Primitive y ) ->
            False

        ( Symbol x, Symbol y ) ->
            x == y

        _ ->
            False


addlikeOp : (Float -> Float -> Float) -> Float -> List Data -> Maybe Data
addlikeOp op zero data =
    case data of
        [] ->
            Just (Number zero)

        (Number x) :: xs ->
            case (addlikeOp op zero xs) of
                Just (Number y_) ->
                    Just (Number (op x y_))

                _ ->
                    Nothing

        _ ->
            Nothing



-- TYPES


type alias Expression =
    Tree Data


type Environment
    = Environment Frame (List Frame)


type Data
    = Number Float
    | Boolean Bool
    | Procedure Procedure
    | Symbol Symbol
    | Primitive Primitive
    | Undefined


type alias ParseState =
    { branches : List (Tree String)
    , done : Maybe (Tree String)
    }


{-| Should symbols bind to Data or Tree Data?
-}
type alias Frame =
    Dict Symbol Data


type alias Procedure =
    ( List Operand, Body )


type alias Primitive =
    List Data -> Maybe Data


type alias Operand =
    Symbol


type alias Value =
    Data


type alias Body =
    Expression


type alias Symbol =
    String



-- REPRESENTATION


coerce : String -> Maybe Data
coerce string =
    let
        coerceSymbol name s =
            s |> maybeIf ((==) name) |> Maybe.map Symbol

        coerceFloat s =
            s |> String.toFloat |> Result.toMaybe |> Maybe.map Number

        coerceBool s =
            case s of
                "True" ->
                    Just (Boolean True)

                "False" ->
                    Just (Boolean False)

                _ ->
                    Nothing

        coerceSymbolic s =
            Just (Symbol s)

        coercePrimitive s =
            primitiveProcedure s |> Maybe.map Primitive

        specialForms =
            [ coerceSymbol "if"
            , coerceSymbol "debug"
            , coerceSymbol "begin"
            , coerceSymbol "set!"
            , coerceSymbol "lambda"
            , coerceSymbol "define"
            , coercePrimitive
            ]

        primitives =
            [ coerceFloat
            , coerceBool
            , coerceSymbolic
            ]
    in
        string |> maybeOr (specialForms ++ primitives)


envGet : Environment -> Symbol -> Maybe Data
envGet env symbol =
    case env of
        Environment base [] ->
            Dict.get symbol base

        Environment base (frame :: rest) ->
            case Dict.get symbol frame of
                Just x ->
                    Just x

                Nothing ->
                    envGet (Environment base rest) symbol


envUpdate : Environment -> Symbol -> Data -> Environment
envUpdate env s x =
    case env of
        Environment base [] ->
            Environment (Dict.insert s x base) []

        Environment base (frame :: rest) ->
            Environment base ((Dict.insert s x frame) :: rest)


envEmpty : Environment
envEmpty =
    Environment Dict.empty []



-- PARSE


{-|
1. parse a list expression by folding over the tokens a function
ParseState -> String -> ParseState. If it encounters a regular symbol, it
appends it to the head of ParseState.branches. If it encounters an open paren,
it makes a new head in ParseState.branches. If it encounters a close paren, it
wraps the head of ParseState.branches in a Branch tag, pops it off, and appends it
to the next branch. Don't need to explicitly track parenthesis depth. If we encounter
an extra close paren, ParseState.branches is empty and we Nothing out. If
parentheses are matched, we end with exactly one item in ParseState.branches
-}
parseToken : String -> ParseState -> Maybe ParseState
parseToken token parseState =
    case parseState.done of
        Just _ ->
            Nothing

        Nothing ->
            case token of
                "(" ->
                    Just { parseState | branches = (Branch []) :: parseState.branches }

                ")" ->
                    case parseState.branches of
                        -- done
                        expr :: [] ->
                            Just { parseState | branches = [], done = Just expr }

                        -- not done
                        (Branch xs) :: (Branch ys) :: rest ->
                            Just { parseState | branches = (Branch ((Branch xs) :: ys)) :: rest }

                        _ ->
                            Nothing

                s ->
                    case parseState.branches of
                        (Branch x) :: rest ->
                            Just { parseState | branches = (Branch ((Leaf s) :: x)) :: rest }

                        [] ->
                            Just { parseState | branches = [ Leaf s ], done = Just (Leaf s) }

                        _ ->
                            Nothing



-- HELPERS


{-| Parse a string of scheme code. Special forms are stored as symbols. Could
pattern match special forms here and produce syntax errors, rather than
evaluation errors in the case structure of eval.
-}
parseScheme : String -> Maybe (Tree String)
parseScheme input =
    let
        padParen : Char -> String -> String
        padParen c s =
            if List.member c [ '(', ')' ] then
                String.concat [ s, " ", String.fromChar c, " " ]
            else
                String.append s (String.fromChar c)
    in
        input
            |> String.toList
            |> List.foldl padParen ""
            |> String.split " "
            |> List.map String.trim
            |> List.filter ((/=) "")
            |> parseTokens


emptyParse : ParseState
emptyParse =
    { branches = [], done = Nothing }


parseTokens : List String -> Maybe (Tree String)
parseTokens tokens =
    foldm parseToken (Just emptyParse) tokens
        |> Maybe.andThen .done
        |> Maybe.map reverseTree


mapTree : (a -> b) -> Tree a -> Tree b
mapTree f tree =
    case tree of
        Leaf x ->
            Leaf (f x)

        Branch xs ->
            Branch (List.map (mapTree f) xs)


filterTree : (a -> Bool) -> Tree a -> Tree a
filterTree f tree =
    let
        g n =
            case n of
                Leaf x ->
                    f x

                Branch _ ->
                    True
    in
        case tree of
            Leaf x ->
                Leaf x

            Branch xs ->
                List.filter g xs
                    |> List.map (filterTree f)
                    |> Branch


traverseTree : (a -> Maybe b) -> Tree a -> Maybe (Tree b)
traverseTree f tree =
    case tree of
        Leaf x ->
            f x |> Maybe.map Leaf

        Branch xs ->
            (Maybe.Extra.traverse (traverseTree f) xs)
                |> Maybe.map Branch


reverseTree : Tree a -> Tree a
reverseTree tree =
    case tree of
        Leaf _ ->
            tree

        Branch xs ->
            Branch (xs |> List.map reverseTree |> List.reverse)


foldm : (a -> b -> Maybe b) -> Maybe b -> List a -> Maybe b
foldm f acc xs =
    case xs of
        [] ->
            acc

        x :: rest ->
            foldm f (Maybe.andThen (f x) acc) rest


maybeIf : (a -> Bool) -> a -> Maybe a
maybeIf test a =
    if (test a) then
        Just a
    else
        Nothing


maybeOr : List (a -> Maybe b) -> a -> Maybe b
maybeOr fs a =
    case fs of
        [] ->
            Nothing

        f :: rest ->
            case f a of
                Just b ->
                    Just b

                Nothing ->
                    maybeOr rest a



-- APP


main : Program Never String Action
main =
    Html.program
        { init = example3 ! []
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.batch [])
        }


init : String
init =
    """
(begin
  (set! x 7)
  (set! f (lambda (a b) (if a b blah)))
  (debug (if True 0 blah))
)
""" |> String.dropLeft 1


example2 : String
example2 =
    """
(begin
  (define (g x) 3)
  (g 7)
)""" |> String.dropLeft 1


example3 : String
example3 =
    """
  (begin
  (set! x 7)
  (set! f (lambda (a b) (if a b blah)))
  (f False x)
)
"""


update : Action -> Model -> ( Model, Cmd msg )
update action model =
    case action of
        Input s ->
            s ! []

        Fuckyou ->
            ("fuckyou" ++ model) ! []


view : String -> Html.Html Action
view input =
    let
        unpack expr =
            case expr of
                Just (Branch exprs) ->
                    exprs

                _ ->
                    []

        expressions =
            "( "
                ++ input
                ++ " )"
                |> parseScheme
                |> unpack
                |> List.filterMap (traverseTree coerce)

        doneExpressions =
            expressions |> List.map (eval envEmpty)

        blah =
            doneExpressions
                |> List.map viewOutput

        viewOutput ( value, environment ) =
            div []
                [ div [] [ value |> toString |> (++) "value: " |> text ]
                , div [] [ environment |> toString |> (++) "environment: " |> text ]
                ]
    in
        div []
            [ textarea [ onInput Input, rows 10, cols 80 ] [ input |> text ]
            , div [] blah
            , div [] [ expressions |> toString |> text ]
            ]


type Action
    = Input String
    | Fuckyou


type alias Model =
    String
