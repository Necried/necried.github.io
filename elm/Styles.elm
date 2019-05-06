module Styles exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Utilities.Spacing as Spacing
import Html exposing (..)
import Html.Attributes exposing (..)
import Markdown exposing (..)


lightGrey =
    "#D3D3D3"


grey =
    "#808080"


dimGrey =
    "#696969"


fontAwesome iconName =
    i [ class <| "fa fa-" ++ iconName, attribute "aria-hidden" "true" ] []

fontAwesomeLink linkName iconName attrs =
    a ([ href linkName, color dimGrey ] ++ attrs ) [ fontAwesome iconName ]

largeBlank =
    Grid.row []
        --this is an empty row (to add blank space)
        [ Grid.col []
            [ h6 [ Spacing.my5 ] [ text " " ] --change spacing number to make empty row bigger or smaller (0-5)
            ]
        ]

        
backgroundImage =
    style "background-image"


backgroundColor =
    style "background-color"


backgroundPosition =
    style "background-position"


backgroundSize =
    style "background-size"


width =
    style "width"


height =
    style "height"


marginTop =
    style "margin-top"


minHeight =
    style "min-height"


minWidth =
    style "min-width"


color =
    style "color"


fontSize =
    style "fontSize"


fontFamily =
    style "fontFamily"


fontStyle =
    style "fontStyle"


myFonts =
    fontFamily "sans-serif"


outline =
    style "outline"

toMD = Markdown.toHtml [ class "content" ]
