module Main exposing (Model, Msg(..), Page(..), Read, ReadContent, aboutView, carouselAboutPage, init, main, navbar, pageTitle, subscriptions, tabsAboutPage, update, view)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Carousel as Carousel
import Bootstrap.Carousel.Slide as Slide
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Navbar as Navbar exposing (..)
import Bootstrap.Progress as Progress
import Bootstrap.Tab as Tab
import Bootstrap.Text as Text
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Size as Size
import Bootstrap.Utilities.Spacing as Spacing
import Browser exposing (..)
import Browser.Navigation as Nav
import Debug exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List exposing (map)
import Markdown exposing (..)
import Styles as Style exposing (..)
import Url as Url


type alias Read =
    String


type alias ReadContent =
    { title : String
    , tableOfContents : List String
    , body : String
    }


type alias AboutState =
    { languageSelection : String }


type Page
    = About AboutState
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
            , page = About { languageSelection = "Haskell" }
            , key = key
            , carouselState = Carousel.initialState
            , tabState = Tab.initialState
            }
    in
    ( initModel, navbarCmd )


type Msg
    = NavbarMsg Navbar.State
    | PageSwitch Page
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | CarouselMsg Carousel.Msg
    | TabMsg Tab.State
    | LanguageSwitch String


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , Carousel.subscriptions model.carouselState CarouselMsg
        , Tab.subscriptions model.tabState TabMsg
        ]


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
            ( { model | carouselState = Carousel.update subMsg model.carouselState }
            , Cmd.none
            )

        TabMsg state ->
            ( { model | tabState = state }
            , Cmd.none
            )

        LanguageSwitch lang ->
            let
                curPage =
                    model.page
            in
            case model.page of
                About aboutState ->
                    ( { model | page = About { aboutState | languageSelection = lang } }
                    , Cmd.none
                    )
                _ ->
                    ( model
                    , Cmd.none
                    )


view : Model -> Document Msg
view model =
    let
        nav =
            navbar model

        title =
            pageTitle True <| model.page

        content =
            aboutView model

        layout =
            Grid.containerFluid [] [ content, largeBlank ]
    in
    { title = title, body = [ CDN.stylesheet, CDN.fontAwesome, nav, layout ] }


navbar : Model -> Html Msg
navbar model =
    let
        aboutDefault =
            case model.page of
                About aboutState -> About aboutState
                _ -> About { languageSelection = "Haskell" }
                           
        pageItems =
            map
                (\page ->
                    let
                        pageStr =
                            pageTitle False page
                    in
                    if model.page == page then
                        Navbar.itemLinkActive [ href pageStr ] [ text pageStr ]

                    else
                        Navbar.itemLink [ href pageStr ] [ text pageStr ]
                )
                [ aboutDefault, Interests, ReadMenu [] ]
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
        About _ ->
            if isTitle then
                "Lucas Dutton"

            else
                "About"

        Interests ->
            "Interests"

        ReadMenu _ ->
            "Reads"

        ReadPage content ->
            content.title



-- About Page


aboutView : Model -> Html Msg
aboutView model =
    let
        languagesList =
            map
                (\( lang, proficiency ) ->
                    ListGroup.li
                        []
                        [ text lang, Progress.progress [ Progress.value proficiency ] ]
                )
                [ ( "Haskell", 90 )
                , ( "Elm", 75 )
                , ( "C++", 70 )
                , ( "C", 70 )
                , ( "Java", 55 )
                , ( "Python", 50 )
                , ( "C#", 25 )
                , ( "Agda", 20 )
                ]

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
    div [] [ carouselAboutPage model, tabsAboutPage model ]


carouselAboutPage model =
    let
        carouselSlide msg =
            Slide.config []
                (Slide.customContent <|
                    text msg
                )

        myBackground =
            [ Style.backgroundImage "url(\"background.jpg\")"
            , Style.backgroundColor "darkBlue"
            , Style.backgroundPosition "center"
            , Style.backgroundSize "cover"
            , Style.minWidth "100%"
            , Style.minHeight "100vh"
            , Style.marginTop "4%"
            ]

        carouselView =
            Grid.container []
                [ Grid.row []
                    [ Grid.col [ Col.attrs [ fontFamily "monospace", fontSize "2em" ] ]
                        [ text "λ x → x" ]
                    ]
                , Grid.row [ Row.attrs [ Style.marginTop "1%" ] ]
                    [ Grid.col []
                        [ Carousel.config CarouselMsg []
                            |> Carousel.slides
                                [ carouselSlide "Software Developer"
                                , carouselSlide "Undergraduate Student"
                                , carouselSlide "Enthusiastic Learner"
                                ]
                            |> Carousel.view model.carouselState
                        ]
                    ]
                , Grid.row [ Row.attrs [ Style.marginTop "1%" ] ]
                    [ Grid.col [ Col.bottomXl ]
                        [ Button.button
                            [ Button.large
                            , Button.dark
                            , Button.onClick <| LinkClicked <| External "http://lucasdutton.website/resume.pdf"
                            ]
                            [ text "Resume" ]
                        ]
                    ]
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
            [ span [] [ carouselView ] ]
        ]


tabsAboutPage model =
    let
        iconCustom =
            [ color grey, fontSize "1.5em" ]

        paneCustom =
            [ Spacing.mt4, color dimGrey ]

        tabViews =
            Tab.config TabMsg
                |> Tab.withAnimation
                |> Tab.fill
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
                                [ h2 [] [ text "Programming Languages" ]
                                , p [] [ languagesSection model.page ]
                                ]
                        }
                    , Tab.item
                        { id = "Projects"
                        , link = Tab.link iconCustom [ Style.fontAwesome "puzzle-piece" ]
                        , pane =
                            Tab.pane paneCustom
                                [ h2 [] [ text "Projects" ]
                                , p [] [ text "This is something completely different." ]
                                ]
                        }
                    , Tab.item
                        { id = "Achievements"
                        , link = Tab.link [ color grey, fontSize "1.85em", fontFamily "monospace" ] [ text "λ x → x" ]
                        , pane =
                            Tab.pane paneCustom
                                [ h2 [] [ text "Identity" ]
                                , p [] [ text "This is something completely different." ]
                                ]
                        }
                    ]
                |> Tab.view model.tabState
    in
    Grid.container [ Style.minHeight "80vh" ]
        [ Grid.row [ Row.bottomXs, Row.attrs [ Style.marginTop "4%" ] ]
            [ Grid.col [] [ span [] [ tabViews ] ] ]
        ]

        
aboutBlurb =
    let
        aboutText = Markdown.toHtml [ class "content" ] """
I'm a third-year software engineering undergraduate studying in McMaster University. I love developing
a wide variety of software, with emphasis on efficiency and safety. On my free time,
I code as a hobby, play video games, and read fictional books and educational resources
on Category Theory, Abstract Algebra and Algorithms. More in **Interests**!
"""
        publications = List.map (\x -> li [] [toMD x])
          [ "[ResearchGate](https://dl.acm.org/author_page.cfm?id=99659321607&coll=DL&dl=ACM&trk=0)"]

        achievements = List.map (\x -> li [] [toMD x])
          [ "[CASCON 2018](https://www-01.ibm.com/ibm/cas/cascon/awards.jsp) - Project of the Year"
          , "[CASCON 2018](https://www-01.ibm.com/ibm/cas/cascon/awards.jsp) - Student of the Year - Honorable Mention"
          , "ECNA ICPC 2018- 31st place"
          ]
   in
   span [ Spacing.mt4 ]
   [ Grid.row [ Row.attrs [Spacing.mt4 ] ]
   [ Grid.col [] [aboutText]
   ]
   , Grid.row [ Row.centerXs, Row.attrs [Spacing.mt4 ] ]
   [ Grid.col [ Col.md6 ] [ h4 [] [ text "Publications" ], span [] [ ul [] publications ] ]
   , Grid.col [ Col.md6 ] [ h4 [] [ text "Achievements" ], span [] [ ul [ ] achievements ] ]
   ]
   ]

languagesSection page =
    let
        selectedLang =
            case page of
                About aboutState -> aboutState.languageSelection
                _ -> "Haskell"
                     
        buttonSelection lang =
            if selectedLang == lang
            then [ ListGroup.dark, ListGroup.attrs [onClick <| LanguageSwitch lang, outline "none", color dimGrey] ]
            else [ ListGroup.attrs [onClick <| LanguageSwitch lang, outline "none", color dimGrey] ]
                
        languagesList =
            map
                (\lang -> ListGroup.button (buttonSelection lang) [ text lang ])
                [ "Haskell"
                , "Elm"
                , "Agda"
                , "C++"
                , "C"
                , "Java"
                , "Python"
                , "C#"
                ]

        selectedBlurb = case selectedLang of
                            "Elm" -> elmBlurb
                            "Agda" -> agdaBlurb
                            _     -> haskellBlurb
                
        haskellBlurb = Markdown.toHtml [] """
Haskell is my go-to language for just about anything. Yes, I am an avid functional programmer, and
I firmly believe that **Programming with Types** is beneficial to the programmer - as the saying goes,
if it compiles, it works!  

#### More Information
- 1 year of research experience of using Haskell for a variety of tasks - Testing, writing and extending DSLs, 
  compilation techniques, faking dependent types.
- Web development, both front-end and back-end, using frameworks such as [Haste](https://haste-lang.org/) and
  [PAL](https://github.com/CSchank/petri-app-land).
- I just enjoy writing code in Haskell in general!
"""

        elmBlurb = Markdown.toHtml [] """
Elm was a language I was introduced to while I had already knew a decent amount of Haskell - and it
has given me the opportunity to do front-end web development in my favourite programming paradigm - 
functional programming!

#### More Information
- This site is a redesign of my old static site, which was made using pure HTML, CSS and Javascript. Woah!
- Elm is surprisingly fast, and competitive with other mainstream front-end Javascript frameworks. See
  [here](https://elm-lang.org/blog/blazing-fast-html-round-two) for the details.
"""

        agdaBlurb = Markdown.toHtml [] """
Mainly used as a theorem prover, Agda is a dependently-typed language which enforces termination of programs.
My intererst in Agda is currently mostly academic in nature.

#### Current Focus
- Learning dependent type theory
- Learning Agda's rich syntax, and standard libraries
- Agda's compiler backend
"""
    in
    Grid.containerFluid [ Spacing.mt4 ]
        [ Grid.row []
            [ Grid.col [ Col.md3 ] [ ListGroup.custom languagesList ]
            , Grid.col [ Col.xs8 ] [ selectedBlurb ]
            ]
        ]
