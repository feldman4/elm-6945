module Scheme exposing (..)

import Html exposing (text, div, textarea)
import Html.Events exposing (onInput)
import Dict exposing (Dict)
import Maybe.Extra


--MORE TYPES


type alias Expression =
    Tree Data


type alias Environment =
    Env


type alias Value =
    Data


type alias Procedure =
    List Data -> Maybe Data


type alias Operand =
    Data


type alias Body =
    Tree Data



--EVAL


eval : Environment -> Expression -> Maybe Value
eval environment expression =
    case expression of
        Branch ((Leaf (Symbol "if")) :: predicate :: consequent :: alternative :: []) ->
            case eval environment predicate of
                Just (Boolean True) ->
                    eval environment consequent

                _ ->
                    eval environment alternative

        _ ->
            Nothing



-- APPLY


apply : Procedure -> Environment -> Maybe Expression
apply procedure environment =
    Nothing


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


coerce : String -> Maybe Data
coerce string =
    let
        coerceIf s =
            s |> maybeIf ((==) "if") |> Maybe.map Symbol

        coerceFloat s =
            s |> String.toFloat |> Result.toMaybe |> Maybe.map Number

        coercions =
            [ coerceIf, coerceFloat ]
    in
        string |> maybeOr coercions



-- TYPES


type alias ParseState =
    { branches : List (Tree String)
    , done : Maybe (Tree String)
    }


type Tree a
    = Branch (List (Tree a))
    | Leaf a


type alias Symbol =
    String


type alias Frame =
    Dict Symbol (Tree String)


type alias Env =
    List Frame


type Data
    = Number Float
    | Boolean Bool
    | Procedure ( List Operand, Body )
    | Symbol String



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


emptyParse : ParseState
emptyParse =
    { branches = [], done = Nothing }


parseTokens : List String -> Maybe (Tree String)
parseTokens tokens =
    foldm parseToken (Just emptyParse) tokens
        |> Maybe.andThen .done
        |> Maybe.map reverseTree


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
        { init = "(lambda (a b) c)" ! []
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.batch [])
        }


update : Action -> Model -> ( Model, Cmd Action )
update action model =
    case action of
        Input s ->
            s ! []


view : String -> Html.Html Action
view input =
    let
        -- coerceTree tree =
        --     traverseTree c
        expressions =
            input
                |> String.split "\n"
                |> List.filter ((/=) "")
                |> List.filterMap parseScheme
                |> List.filterMap (traverseTree coerce)
                |> Debug.log "input expressions"
                |> List.map (eval [])

        blah =
            expressions
                |> List.map toString
                |> List.map (\s -> div [] [ text s ])
    in
        div []
            [ textarea [ onInput Input ] [ input |> text ]
            , div [] blah
            ]


type Action
    = Input String


type alias Model =
    String
