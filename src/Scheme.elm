module Scheme exposing (..)

import Html exposing (text, div, textarea)
import Html.Attributes exposing (height, width, rows, cols)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import Maybe.Extra


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


evalProcedure : Expression -> List Expression -> Environment -> Maybe ( Procedure, List Value, Environment )
evalProcedure =
    evalProcedureApplicative



--MORE TYPES
--EVAL


{-| (Maybe Value, Environment) is not as nice as Maybe (Value, Environment).
But it does let you signal that things like set! don't return a defined value,
without introducing Data = ... | Undefined.
-}
eval : Environment -> Expression -> ( Maybe Value, Environment )
eval environment expression =
    case expression of
        Leaf (Boolean x) ->
            ( Just (Boolean x), environment )

        Leaf (Number x) ->
            ( Just (Number x), environment )

        Leaf (Symbol s) ->
            ( envGet environment s, environment )

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
            case evalProcedure operator arguments environment of
                Just ( procedure, argumentData, env2 ) ->
                    eval env2 <| apply procedure argumentData

                _ ->
                    ( Nothing, environment )

        _ ->
            ( Nothing, environment )


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


evalProcedureApplicative :
    Expression
    -> List Expression
    -> Environment
    -> Maybe ( Procedure, List Value, Environment )
evalProcedureApplicative operator arguments environment =
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
        case eval environment operator of
            ( Just (Procedure p), env2 ) ->
                List.foldl f (Just ( [], env2 )) arguments
                    |> Maybe.map (\( a, b ) -> ( p, a, b ))

            _ ->
                Nothing


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


{-| needs to substitute values for symbols in the tree
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
primitiveProcedure : String -> Maybe (List Data -> Maybe Data)
primitiveProcedure name =
    case name of
        "+" ->
            Just (addlikeOp (+))

        "*" ->
            Just (addlikeOp (*))

        _ ->
            Nothing


addlikeOp : (Float -> Float -> Float) -> List Data -> Maybe Data
addlikeOp op data =
    case data of
        [] ->
            Just (Number 0)

        (Number x) :: xs ->
            case (addlikeOp op xs) of
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


{-| Should symbols bind to Data or Expression?
-}
type alias Frame =
    Dict Symbol Data


type alias Procedure =
    ( List Operand, Body )


type alias Operand =
    Symbol


type alias Value =
    Data


type alias Body =
    Expression


type alias Symbol =
    String


type Tree a
    = Branch (List (Tree a))
    | Leaf a


type alias ParseState =
    { branches : List (Tree String)
    , done : Maybe (Tree String)
    }



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

        specialForms =
            [ coerceSymbol "if"
            , coerceSymbol "debug"
            , coerceSymbol "begin"
            , coerceSymbol "set!"
            , coerceSymbol "lambda"
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


{-| parse a string of scheme code, parentheses
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
        { init = init ! []
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.batch [])
        }


init : String
init =
    """
(begin
  (set! x 7)
  (debug (if True x 4))
  ( (lambda (a b) (if a b 4)) True 7 )
)

    """ |> String.dropLeft 1


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Input s ->
            s ! []


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
                |> Debug.log "unpack"
                |> List.filterMap (traverseTree coerce)
                -- |> Debug.log "input expressions"
                |>
                    List.map (eval envEmpty)

        blah =
            expressions
                |> List.map toString
                |> List.map (\s -> div [] [ text s ])
    in
        div []
            [ textarea [ onInput Input, rows 10, cols 80 ] [ input |> text ]
            , div [] blah
            ]


type Action
    = Input String


type alias Model =
    String
