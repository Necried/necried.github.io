module Main exposing (..)

import Styles as Style exposing (..)

import Browser exposing (..)
import Browser.Navigation as Nav

import Bootstrap.Button as Button
import Bootstrap.Grid.Col as Col
import Bootstrap.CDN as CDN
import Bootstrap.Carousel as Carousel
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Progress as Progress
import Bootstrap.Carousel.Slide as Slide
import Bootstrap.Navbar as Navbar exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Text as Text
import Bootstrap.Tab as Tab
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Size as Size

import Markdown exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)

import Url as Url

import List exposing (map)

import Debug exposing (..)

type alias Read = String

type alias ReadContent =
    { title : String
    , tableOfContents : List String
    , body : String
    }
    
type Page =
    About
    | Interests
    | ReadMenu (List Read)
    | ReadPage ReadContent

type alias Model =
    { navbarState : Navbar.State
    , page : Page
    , key : Nav.Key
    , carouselState : Carousel.State
    , tabState : Tab.State
    }

init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( navbarState, navbarCmd ) =
            Navbar.initialState NavbarMsg
        initModel =
            { navbarState = navbarState
            , page = About
            , key = key
            , carouselState = Carousel.initialState
            , tabState = Tab.initialState
            }
    in
    ( initModel, navbarCmd )

type Msg =
      NavbarMsg Navbar.State
    | PageSwitch Page
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CarouselMsg Carousel.Msg
    | TabMsg Tab.State
      
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , Carousel.subscriptions model.carouselState CarouselMsg
        , Tab.subscriptions model.tabState TabMsg]

main =
  Browser.application
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    , onUrlRequest = LinkClicked
    , onUrlChange = UrlChanged
    }
      
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )
        PageSwitch page ->
            ( { model | page = page }, Cmd.none )
        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )                    
                External url ->
                    ( model
                    , Nav.load url
                    )
        UrlChanged url ->
            todo "todo"
        CarouselMsg subMsg ->
            ({ model | carouselState = Carousel.update subMsg model.carouselState },
                 Cmd.none)
        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )
            
view : Model -> Document Msg
view model =
    let
        nav = navbar model
        title = pageTitle True <| model.page
        content = aboutView model
                
        layout = Grid.containerFluid [] [content]         
    in
        { title = title, body = [CDN.stylesheet, CDN.fontAwesome, nav, layout] }

navbar : Model -> Html Msg
navbar model =
    let
        pageItems =
            map (\page ->
                     let pageStr = pageTitle False page
                     in if model.page == page
                          then Navbar.itemLinkActive [ href pageStr ] [ text pageStr ]
                          else Navbar.itemLink [ href pageStr ] [ text pageStr ]
                ) [About, Interests, ReadMenu []]
    in
        Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.brand [ href "Lucas Dutton" ] [ text "Lucas Dutton" ]
            |> Navbar.items pageItems
            |> Navbar.dark
            |> Navbar.fixTop
            |> Navbar.view model.navbarState            

pageTitle : Bool -> Page -> String
pageTitle isTitle page =
    case page of
        About -> if isTitle then "Lucas Dutton" else "About"
        Interests -> "Interests"
        ReadMenu _ -> "Reads"
        ReadPage content -> content.title

-- About Page
                            
aboutView : Model -> Html Msg
aboutView model =
    let
        languagesList =
            map (\(lang, proficiency) ->
                     ListGroup.li
                     []
                     [ text lang, Progress.progress [ Progress.value proficiency ] ]
                )
            [("Haskell", 90), ("Elm", 75), ("C++", 70), ("C", 70), ("Java", 55),
            ("Python", 50), ("C#", 25), ("Agda", 20)]
            
        languageCard =
            Card.config [ Card.outlineInfo, Card.attrs [ Spacing.mb5 ] ]
                |> Card.headerH1 [] [ text "Languages" ]
                |> Card.block []
                   [ Block.titleH1 [] [ text "Block title" ]
                   , Block.text [] [ text "Some block content" ]
                   , Block.link [ href "#" ] [ text "MyLink" ]
                   ]
                |> Card.view
    in
        div [] [carouselAboutPage model, tabsAboutPage model]

carouselAboutPage model =
    let
        carouselSlide msg =
            Slide.config []
                (Slide.customContent
                     <| text msg
                )
        myBackground =
            [Style.backgroundImage "url(\"background.jpg\")"
            ,Style.backgroundColor "darkBlue"
            ,Style.backgroundPosition "center"
            ,Style.backgroundSize "cover"
            ,Style.minWidth "100%"
            ,Style.minHeight "100vh"
            ,Style.marginTop "4%"
            ]

        carouselView =
            Grid.container []
                [ Grid.row []
                  [ Grid.col [ Col.attrs [fontFamily "monospace", fontSize "2em" ] ]
                        [ text "λ x → x" ]
                  ]
                , Grid.row [ Row.attrs [Style.marginTop "1%"] ] 
                  [ Grid.col []
                        [Carousel.config CarouselMsg []
                        |> Carousel.slides
                             [carouselSlide "Software Developer"
                             , carouselSlide "Undergraduate Student"
                             , carouselSlide "Enthusiastic Learner"
                             ]
                        |> Carousel.view model.carouselState
                        ]
                  ]
                , Grid.row [ Row.attrs [Style.marginTop "1%"]]
                    [ Grid.col [Col.bottomXl]
                          [Button.button [ Button.large, Button.dark
                                         , Button.onClick <| LinkClicked <| External "http://lucasdutton.website/resume.pdf" ]
                               [ text "Resume" ]
                          ]
                    ]
                ]

        textStyle =
            [color lightGrey
            ,fontSize "30px"
            ,fontStyle "bold"
            ,myFonts]
    in
       Grid.row
            [ Row.bottomXs, Row.attrs myBackground ]
            [ Grid.col [ Col.middleXs, Col.textAlign Text.alignXsCenter, Col.attrs textStyle ]
                [ span [] [ carouselView ] ]
            ]

tabsAboutPage model =
    let
        aboutBlurb = Markdown.toHtml [class "content"] """
I'm a third-year software engineering undergraduate studying in McMaster University. I love developing
a wide variety of software, with emphasis on efficiency and safety. On my free time,
I code as a hobby, play video games, and read fictional books and educational resources
on Category Theory, Abstract Algebra and Algorithms.
"""
        iconCustom = [ color grey, fontSize "1.5em" ]
        paneCustom = [ Spacing.mt3, color dimGrey ]
        tabViews = Tab.config TabMsg
                 |> Tab.withAnimation
                 |> Tab.justified
                 |> Tab.items
                    [ Tab.item
                          { id = "About"
                          , link = Tab.link iconCustom [ Style.fontAwesome "info-circle" ]
                          , pane =
                              Tab.pane paneCustom
                                  [ h2 [] [ text "About" ]
                                  , aboutBlurb
                                  ]
                          }
                    , Tab.item
                        { id = "Programming Languages"
                        , link = Tab.link iconCustom [ Style.fontAwesome "code" ]
                        , pane =
                            Tab.pane paneCustom
                                [ h2 [] [ text  "Programming Languages" ]
                                , p [] [ text "This is something completely different." ]
                                ]
                        }
                   , Tab.item
                        { id = "Projects"
                        , link = Tab.link iconCustom [ Style.fontAwesome "puzzle-piece" ]
                        , pane =
                            Tab.pane paneCustom
                                [ h2 [] [ text  "Projects" ]
                                , p [] [ text "This is something completely different." ]
                                ]
                        }
                   , Tab.item
                        { id = "Achievements"
                        , link = Tab.link iconCustom [ Style.fontAwesome "trophy" ]
                        , pane =
                            Tab.pane paneCustom
                                [ h2 [] [ text  "Achievements" ]
                                , p [] [ text "This is something completely different." ]
                                ]
                        }
                    ]
                 |> Tab.view model.tabState
    in
        Grid.container [  ]
            [ Grid.row [ Row.bottomXs, Row.attrs [Style.marginTop "4%" ] ]
                 [Grid.col [ ] [ span [] [tabViews] ]]
            ]
        
