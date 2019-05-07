module Styles exposing (..)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
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

externalLink : String -> String -> Html msg
externalLink url label =
    a [ href url, target "_blank" ]
        [ text label ]

type HeroHeight = Half | Full

heroHeader : HeroHeight -> Html msg -> Html msg
heroHeader heroHeight content =
    let
        minHeightVal =
            case heroHeight of
                Full -> "95.5vh"
                Half -> "50vh"
                        
        myBackground =
            [ backgroundImage "url(\"background.jpg\")"
            , backgroundColor "darkBlue"
            , backgroundPosition "center"
            , backgroundSize "cover"
            , minWidth "100%"
            , minHeight minHeightVal
            , marginTop "3.55%"
            ]

        textStyle =
            [ color lightGrey
            , fontSize "30px"
            , fontStyle "bold"
            , myFonts
            ]

    in
        Grid.row
            [ Row.bottomXs, Row.attrs myBackground ]
            [ Grid.col [ Col.middleXs, Col.textAlign Text.alignXsCenter, Col.attrs textStyle ]
                  [ span [] [ content ] ]
            ]    

pageNotFoundView : Html msg
pageNotFoundView =
    let
        myBackground =
            [ backgroundPosition "center"
            , backgroundSize "cover"
            , minWidth "100%"
            , minHeight "50%"
            , marginTop "3.55%"
            ]
            
        textStyle fsize =
            [ fontSize fsize
            , fontStyle "bold"
            , color dimGrey
            , myFonts
            ]
    in
    Grid.row [ Row.bottomXs, Row.attrs myBackground ]
        [ Grid.col [ Col.middleXs, Col.textAlign Text.alignXsCenter ]
                  [ h1 (textStyle "100px") [ text "404 Not Found"]
                  , p  (textStyle "20px") [ text "Either the link is bad, or the page is under construction." ]
                  ]
        ]
        
pageLoading : Html msg
pageLoading =
    let
        myBackground =
            [ backgroundPosition "center"
            , backgroundSize "cover"
            , minWidth "100%"
            , minHeight "50%"
            , marginTop "3.55%"
            ]
            
        textStyle fsize =
            [ fontSize fsize
            , fontStyle "bold"
            , color dimGrey
            , myFonts
            ]
    in
    Grid.row [ Row.bottomXs, Row.attrs myBackground ]
        [ Grid.col [ Col.middleXs, Col.textAlign Text.alignXsCenter ]
                  [ h1 (textStyle "100px") [ text "Loading page..."]
                  ]
        ]
