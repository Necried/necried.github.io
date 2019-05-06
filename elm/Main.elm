module Main exposing (..)

import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Carousel as Carousel
import Bootstrap.Carousel.Slide as Slide
import Bootstrap.General.HAlign as HAlign
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Navbar as Navbar exposing (..)
import Bootstrap.Progress as Progress
import Bootstrap.Tab as Tab
import Bootstrap.Text as Text
import Bootstrap.Utilities.Display as Display
import Bootstrap.Utilities.Flex as Flex
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
import Url as Url

import Styles as Style exposing (..)
import Styles exposing (externalLink)
import Routes exposing (..)

type alias ReadContent =
    { title : String
    , tableOfContents : List String
    , body : String
    }


type alias AboutState =
    { languageSelection : String }


type alias Model =
    { navbarState : Navbar.State
    , page : Page
    , key : Nav.Key
    , carouselState : Carousel.State
    , tabState : Tab.State
    , aboutState : AboutState
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
            , aboutState = { languageSelection = "Haskell" }
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
            urlUpdate url model

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
                About ->
                    ( { model | aboutState = { languageSelection = lang } }
                    , Cmd.none
                    )

                _ ->
                    ( model
                    , Cmd.none
                    )

urlUpdate : Url.Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case Routes.decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just page ->
            ( { model | page = page }, Cmd.none )

view : Model -> Document Msg
view model =
    let
        nav =
            navbar model

        title =
            pageTitle True <| model.page

        content =
            case model.page of
                Interests ->
                    interests

                _ ->
                    aboutView model

        layout =
            Grid.containerFluid [] [ content, largeBlank ]
    in
    { title = title, body = [ CDN.fontAwesome, nav, layout, viewFooter ] }


navbar : Model -> Html Msg
navbar model =
    let
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
                [ About, Interests, ReadMenu ]
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
        About ->
            if isTitle then
                "Lucas Dutton"

            else
                "About"

        Interests ->
            "Interests"

        ReadMenu ->
            "Reads"

        ReadPage content ->
            content

        Home ->
            ""

        NotFound ->
            "404 Not Found"
                
viewFooter : Html Msg
viewFooter =
    let
        links =
            ul [ class "bd-footer-links" ]
                    [ li []
                          [ Style.fontAwesome "github"
                          , span [ Spacing.mr1 ] []
                          , Style.externalLink "https://github.com/Necried" "Github"
                          ]
                    , li []
                          [ Style.fontAwesome "linkedin"
                          , span [ Spacing.mr1 ] []
                          , Style.externalLink "https://www.linkedin.com/in/lucas-matthew-dutton-50061b133/" "LinkedIn"
                          ]
                    , li []
                          [ Style.fontAwesome "hackerrank"
                          , span [ Spacing.mr1 ] []
                          , Style.externalLink "https://www.hackerrank.com/luke97" "HackerRank"
                          ]
                    ]
            
    in
    footer [ class "bd-footer", backgroundColor lightGrey ]
        [ Grid.containerFluid [  ]
              [ Grid.row []
                    [ Grid.col [ Col.offsetMd2, Col.attrs [fontSize "18px"] ] [ text "Connect with me" ] ]
              , Grid.row []
                    [ Grid.col [ Col.offsetMd2 ] [ links ]
                    ]
              , Grid.row []
                    [ Grid.col [ Col.offsetMd2 ] [ text "Developed with Elm 0.19 and elm-bootstrap" ]
                    ]
              ]
        ]

{-              
              [ ul [ class "bd-footer-links" ]
                    [ li []
                          [ i [ class "fa fa-github", attribute "aria-hidden" "true" ] []
                          , Style.externalLink "https://github.com/Necried" "Github"
                          ]
                    , li []
                          [ fontAwesome "linkedin"
                          , Style.externalLink "https://www.linkedin.com/in/lucas-matthew-dutton-50061b133/" "LinkedIn"
                          ]
                    ]
              , p []
                  [ text "Developed with Elm 0.19 and elm-bootstrap" ]
              ]
        ]
-}
-- About Page


aboutView : Model -> Html Msg
aboutView model =
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
            , Style.minHeight "95.5vh"
            , Style.marginTop "3.55%"
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
                                , p [] [ languagesSection model ]
                                ]
                        }
                    , Tab.item
                        { id = "Projects"
                        , link = Tab.link iconCustom [ Style.fontAwesome "puzzle-piece" ]
                        , pane =
                            Tab.pane paneCustom
                                [ h2 [] [ text "Projects" ]
                                , projectsSection model.page
                                ]
                        }
                    , Tab.item
                        { id = "Achievements"
                        , link = Tab.link [ color grey, fontSize "1.85em", fontFamily "monospace" ] [ text "λ x → x" ]
                        , pane =
                            Tab.pane paneCustom
                                [ h2 [] [ text "Identity" ]
                                , p [] [ lambda ]
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
        aboutText =
            Markdown.toHtml [ class "content" ] """
I'm a third-year software engineering undergraduate studying in McMaster University. I love developing
a wide variety of software, with emphasis on efficiency and safety. On my free time,
I code as a hobby, play video games, and read fictional books and educational resources
on Category Theory, Abstract Algebra and Algorithms. More in **Interests**!
"""

        publications =
            List.map (\x -> li [] [ toMD x ])
                [ "[ResearchGate](https://dl.acm.org/author_page.cfm?id=99659321607&coll=DL&dl=ACM&trk=0)" ]

        achievements =
            List.map (\x -> li [] [ toMD x ])
                [ "[CASCON 2018](https://www-01.ibm.com/ibm/cas/cascon/awards.jsp) - Project of the Year"
                , "[CASCON 2018](https://www-01.ibm.com/ibm/cas/cascon/awards.jsp) - Student of the Year - Honorable Mention"
                , "ECNA ICPC 2018- 31st place"
                ]
    in
    span [ Spacing.mt4 ]
        [ Grid.row [ Row.attrs [ Spacing.mt4 ] ]
            [ Grid.col [] [ aboutText ]
            ]
        , Grid.row [ Row.centerXs, Row.attrs [ Spacing.mt4 ] ]
            [ Grid.col [ Col.md6 ] [ h4 [] [ text "Publications" ], span [] [ ul [] publications ] ]
            , Grid.col [ Col.md6 ] [ h4 [] [ text "Achievements" ], span [] [ ul [] achievements ] ]
            ]
        ]


languagesSection model =
    let
        page = model.page
               
        selectedLang =
            case page of
                About ->
                    model.aboutState.languageSelection

                _ ->
                    "Haskell"

        buttonSelection lang =
            if selectedLang == lang then
                [ ListGroup.dark, ListGroup.attrs [ onClick <| LanguageSwitch lang, outline "none", color dimGrey ] ]

            else
                [ ListGroup.attrs [ onClick <| LanguageSwitch lang, outline "none", color dimGrey ] ]

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

        selectedBlurb =
            case selectedLang of
                "Elm" ->
                    elmBlurb

                "Agda" ->
                    agdaBlurb

                "C++" ->
                    cppBlurb

                "C" ->
                    cBlurb

                "Java" ->
                    javaBlurb

                "Python" ->
                    pythonBlurb

                "C#" ->
                    cSharpBlurb

                _ ->
                    haskellBlurb

        haskellBlurb =
            Markdown.toHtml [] """
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

        elmBlurb =
            Markdown.toHtml [] """
Elm was a language I was introduced to while I had already knew a decent amount of Haskell - and it
has given me the opportunity to do front-end web development in my favourite programming paradigm -
functional programming!

#### More Information
- This site is a redesign of my old static site, which was made using pure HTML, CSS and Javascript. Woah!
- Elm is surprisingly fast, and competitive with other mainstream front-end Javascript frameworks. See
  [here](https://elm-lang.org/blog/blazing-fast-html-round-two) for the details.
"""

        agdaBlurb =
            Markdown.toHtml [] """
Mainly used as a theorem prover, Agda is a dependently-typed language which enforces termination of programs.
My intererst in Agda is currently mostly academic in nature.

#### Current Focus
- Learning dependent type theory
- Learning Agda's rich syntax, and standard libraries
- Agda's compiler backend
"""

        cppBlurb =
            Markdown.toHtml [] """
C++ is a language I mainly use for competitive programming. It has a rich standard library which contains
many important data structures and algorithms, and compiles to really fast code, all which are essential
for my application.

#### More Information
- I have also taken a course in McMaster that used C++ to teach software development practices.
- C++ is one of my go-to object-oriented programming language, the other being Python.
"""

        cBlurb =
            Markdown.toHtml [] """
C is one of the fundamental languages that taught me about low-level computer intrinsics. I have since
written code in C to deliver algorithmic implementations of my research work to IBM, and taught
a course that used C as the language of choice to teach principles of programming to second-year
students in McMaster University.

#### More Information
- I have both taken and taught McMaster's Software Engineering 2S03 course that uses C.
- One of the programs that I wrote in C involved image filtering- applying mean and median filters to images
  using highly optimized code.
"""

        javaBlurb =
            Markdown.toHtml [] """
Mainly used for completing a project in McMaster's Software 2XB3 course.

#### More Information
- Coursework based on finding optimal paths for Uber drivers in cities, using
machine learning and graph algorithms.
"""

        pythonBlurb =
            Markdown.toHtml [] """
The first language that I learnt. I now use Python for a variety of applications,
from scripting to data visualization.

#### More Information
- Used Python libraries for visualizing data on floating point errors to understand
the behaviour of our floating point implementation on different intervals during
my IBM research work.
- Frequently used to solve specific competitive programming problems, such as String
and BigInteger problems.
"""

        cSharpBlurb =
            Markdown.toHtml [] """
Mainly used when I was developing games with Unity.
"""
    in
    Grid.containerFluid [ Spacing.mt4 ]
        [ Grid.row []
            [ Grid.col [ Col.md3 ] [ ListGroup.custom languagesList ]
            , Grid.col [ Col.xs8 ] [ selectedBlurb ]
            ]
        ]


projectsSection page =
    let
        finsm =
            ListGroup.li
                [ ListGroup.dark, ListGroup.attrs [ Flex.col, Flex.alignItemsStart, color dimGrey ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ h5 [ Spacing.mb1 ] [ text "finsm.io" ]
                    , h5 []
                        [ Style.fontAwesomeLink "https://finsm.io" "link" [Spacing.mr2]
                        , Style.fontAwesomeLink "https://github.com/CSchank/finsm" "github" [] ]
                    ]
                , p [ Spacing.mb1 ] [ finsmBlurb ]
                ]

        newYouth =
            ListGroup.li
                [ ListGroup.light, ListGroup.attrs [ Flex.col, Flex.alignItemsStart, color dimGrey ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ h5 [ Spacing.mb1 ] [ text "NewYouthApp" ]
                    , h5 [] [ Style.fontAwesomeLink "https://macoutreach.rocks/newyouth/" "link" [] ]
                    ]
                , p [ Spacing.mb1 ] [ newYouthBlurb ]
                ]

        calcCheck =
            ListGroup.li
                [ ListGroup.dark, ListGroup.attrs [ Flex.col, Flex.alignItemsStart, color dimGrey ] ]
                [ div [ Flex.block, Flex.justifyBetween, Size.w100 ]
                    [ h5 [ Spacing.mb1 ] [ text "CalcCheckWeb" ]
                    , h5 [] [ Style.fontAwesomeLink "http://calccheck.mcmaster.ca" "link" [] ]
                    ]
                , p [ Spacing.mb1 ] [ calcCheckBlurb ]
                ]

        finsmBlurb =
            Markdown.toHtml [] """
A lightweight app made with Elm and the GraphicsSVG library to build, simulate and export finite
state machines. Successfully used in McMaster's second year Finite Automata course by students to submit assignments.
[finsm.io](finsm.io) was developed as an open-source project in conjunction with another McMaster student.
"""

        newYouthBlurb =
            Markdown.toHtml [] """
Developed with McMaster Outreach/McMaster Start Coding students, this app was built from the
ground up, beginning from the web framework, [Petri-App-Land](https://github.com/CSchank/petri-app-land).
Using Elm and Haskell, we worked with Brampton Multicultural Centre to provide an application for
new youths to provide resources and help for integrating into Canada.
"""

        calcCheckBlurb =
            Markdown.toHtml [] """
A theorem prover developed by Dr. Wolfram Kahl, used by second-year students in the Discrete Mathematics
course. I contributed some front-end improvements and suggestions for the web interface.
"""
    in
    Grid.container [ Spacing.mt4, Size.w100 ]
        [ ListGroup.ul [ finsm, newYouth, calcCheck ] ]

lambda = Markdown.toHtml [] """
Hello there! Wondering what that weird mathematical equation was, complete with that cool looking λ symbol is? Read on!

### Lambda Calculus

The Lambda Calculus is a model of computation introduced by Alonzo Church. Its syntax consists of three simple
rules of construction:

- Variables: Strings or characters representing some parameter of a value
- Abstraction: A function definition, with the syntax (λ x → M). M is another expression, and x is bound in M.
- Application: Applying functions to arguments, e.g. (M N) is "M is applied to N".

### Identities

Identities are a vital part of mathematics; One of them is the identity function, defined as 
`f(x) = x`. This means that whatever value we substitute for `x`, the function will return
the same output as our input! 

If you now see the correspondence between `f(x) = x`, and `λ x → x`, then yes! We've just seen
the identity function written with the lambda calculus!

##### What's the point?

I like to think that this website displays my identity, hence my use for this function :)

"""

interests : Html msg
interests =
    iframe
    [ style "display" "inline"
    , Style.minWidth "100%"
    , Style.minHeight "200vh"
    , src "assets/content/Interests.html"
    ]
    [ ]
    
