module Reads exposing (..)

import Bootstrap.Button as Button
import Bootstrap.Card.Block as Block
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

renderCard : Entry -> (String -> msg) -> Html msg
renderCard e fmsg =
    Card.config []
        |> Card.header [ class "text-center" ]
           [ img [ src e.imgsrc ] []
           , h3 [ Spacing.mt2 ] [ text e.title ]
           ]
        |> Card.block []
           [ Block.text [] [ text <| "Created at: " ++ e.date ]
           , Block.custom <|
               Button.linkButton [ Button.dark, Button.attrs [ href e.url ], Button.onClick <| fmsg e.title ]
                   [ text "Read" ]
           ]
        |> Card.view

renderCards : (String -> msg) -> List (List Entry) -> List (Html msg)
renderCards fmsg entries =
    let
        renderCols inp =
            case inp of
                [] -> []
                (x :: rest) ->
                    Grid.col [ Col.md5 ] [ renderCard x fmsg ] :: renderCols rest
    in
    case entries of
        [] -> []
        (xs :: xxs) ->
            Grid.row [ Row.attrs [Spacing.mt3] ] (renderCols xs) :: renderCards fmsg xxs
                

reads = [{ title = "List of Very Incomplete,Need-to-read Resources", date = "1st March, 2019", url = "/resources.md", imgsrc = "https://images.freeimages.com/images/small-previews/535/natural-wonders-1400924.jpg" }]