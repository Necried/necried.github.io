module Reads exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card as Card
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Utilities.Spacing as Spacing

import Html exposing (..)
import Html.Attributes exposing (..)

import Utils exposing (..)
import Styles as Style

type alias Entry =
    { title : String
    , date  : String
    , url : String
    , imgsrc : String
    }

renderCard : Entry -> Html msg
renderCard e =
    Card.config []
        |> Card.header [ class "text-center" ]
           [ img [ src e.imgsrc ] []
           , h3 [ Spacing.mt2 ] [ text e.title ]
           ]
        |> Card.block []
           [ Block.text [ text "Created at: " ++ e.date ]
           , Block.custom <| Button.button [ Button.dark ] [ text "Read" ]
           ]
        |> Card.view

renderCards : List (List Entry) -> List (Html msg)
renderCards entries =
    let
        renderCols inp =
            case inp of
                [] -> []
                (x :: rest) ->
                    Grid.col [ Col.md5 ] [ renderCard x ] :: renderCols rest
    in
    case entries of
        [] -> []
        (xs :: xxs) ->
            Grid.row [ Row.attrs [Spacing.mt3] ] (renderCols xs) :: renderCards xxs
                
