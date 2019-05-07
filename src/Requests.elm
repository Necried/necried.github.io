module Requests exposing (..)

import Http
import Routes exposing (..)

contentPages = [ Interests, ReadMenu ]

getRequest : Page -> (Page -> Result Http.Error String -> msg) -> Cmd msg
getRequest page for =
    if not <| List.member page contentPages
        then Cmd.none
    else
        Http.get
            { url = urlTranslation page
            , expect = Http.expectString <| for page 
            }
    
urlTranslation : Page -> String
urlTranslation page =
    case page of
        Interests ->
            "https://raw.githubusercontent.com/Necried/necried.github.io/elm-rewrite/assets/content/Interests.md"

        _ ->
            ""
