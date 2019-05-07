module Routes exposing (..)

import Html
import Html.Attributes as Attr
import Html.Events as Events exposing (preventDefaultOn)
import Json.Decode
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, custom, fragment, s, top, string)

type alias ReadTitle =
    String
        
type Page
    = Home
    | About
    | Interests
    | ReadMenu
    | ReadPage ReadTitle
    | NotFound

routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map About (UrlParser.oneOf [s "About", s "Lucas%20Dutton", s "index.html"] )
        , UrlParser.map Interests (s "Interests")
        , UrlParser.map ReadMenu (s "Reads")
        , UrlParser.map ReadPage (string)
        ]

decode : Url -> Maybe Page
decode url =
    UrlParser.parse routeParser url

encode : Page -> String
encode route =
    case route of
        Home ->
            "/"

        About ->
            "/About"

        Interests ->
            "/Interests"

        ReadMenu ->
            "/Reads"

        ReadPage title ->
            "/" ++ title

        NotFound ->
            "/Notfound"        
