module Scheme.App exposing (..)

import Html exposing (text, div, textarea, li, ul)
import Html.Attributes exposing (height, width, rows, cols)
import Html.Events exposing (onInput, onClick)
import Scheme.Types exposing (..)
import Scheme exposing (..)
import Scheme.View exposing (..)
import Scheme.Extra exposing (evalThrough, drawNodeData)


-- APP


main : Program Never String Action
main =
    Html.program
        { init = appProgram ! []
        , update = update
        , view = view
        , subscriptions = (\_ -> Sub.batch [])
        }


init : String
init =
    """
(begin
  (set! x 7)
  (set! f (lambda (a b) (if a b -99)))
  (debug (if True 0 blah))
)
""" |> String.trim


example2 : String
example2 =
    """
(begin
  (define (g x) 3)
  (g 7)
)""" |> String.trim


example3 : String
example3 =
    """
  (begin
  (set! x 7)
  (set! f (lambda (a b) (if a b 9)))
  (f False x)
)
""" |> String.trim


fibProgram : String
fibProgram =
    """(begin
  (define (fib n)
    (if (= n 1 )
        1
        (if (= n 2)
          1
          (+ (fib (- n 1)) (fib (- n 2)))
  )))
(fib 3)
)

""" |> String.trim


appProgram : String
appProgram =
    """
  (begin
  (define (f x) (+ 1 -99))
  (+ 3 ( f 1) )
)
""" |> String.trim


choices : List ( String, String )
choices =
    [ ( "A", example2 ), ( "B", example3 ), ( "D", appProgram ), ( "fib", fibProgram ) ]


update : Action -> Model -> ( Model, Cmd msg )
update action model =
    case action of
        Input s ->
            s ! []

        Fuckyou ->
            ("fuckyou" ++ model) ! []


extraView : Expression -> Html.Html msg
extraView expression =
    let
        fullEval =
            evalThrough envEmpty expression

        noBegin x =
            case x of
                Symbol "begin" ->
                    False

                _ ->
                    True

        trees =
            fullEval
                |> Tuple.first
                |> List.reverse
                |> List.map (\( env, expr ) -> viewTree drawNodeData (Symbol ".") (expr |> filterTree noBegin))
    in
        div [] trees


viewChoices : List ( String, String ) -> Html.Html Action
viewChoices choices =
    let
        f ( label, program ) =
            li [ onClick (Input program) ] [ label |> text ]
    in
        ul [] (List.map f choices)


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
                |> Debug.log "program"

        viewOutput ( value, environment ) =
            div []
                [ div [] [ value |> toString |> (++) "value: " |> text ]
                , div [] [ environment |> toString |> (++) "environment: " |> text ]
                ]
    in
        div []
            [ viewChoices choices
            , textarea [ onInput Input, rows 10, cols 80, Html.Attributes.value input ] []
            , div [] (List.map extraView expressions)
            ]
