module Styles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

lightGrey = "#D3D3D3"
grey = "#808080"
dimGrey = "#696969"
fontAwesome iconName = i [ class <| "fa fa-" ++ iconName, attribute "aria-hidden" "true" ] []
                        
backgroundImage = style "background-image"
backgroundColor = style "background-color"
backgroundPosition = style "background-position"
backgroundSize = style "background-size"
width = style "width"
height = style "height"
marginTop = style "margin-top"
minHeight = style "min-height"
minWidth  = style "min-width"
color = style "color"
fontSize = style "fontSize"
fontFamily = style "fontFamily"
fontStyle = style "fontStyle"
            
myFonts = fontFamily "sans-serif"
