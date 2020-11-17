{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}

module HelpModals (ModalPage(..), viewHelpModal, Interface(..), Action(GoTo), updateHelpModals) where

import           Control.Lens
import           Helpers
import           Miso
import           Miso.String
data Action =
      GoTo ModalPage
    deriving (Eq, Show)

data ModalPage =
    MainPage
    | IntroArchitecturePage
    | IntroRegMemPage
    | IntroProgPage
    | IntroExecutionPage
    | GuideFilePage
    | GuideExecPage
    | AvailableInstructions
    deriving (Eq, Show)

newtype Model =
  Model { _pageModel :: ModalPage } deriving (Eq, Show)

makeLenses ''Model

data Interface action =
  Interface
    { passAction :: Action -> action
    , exitModal  :: action
    }

updateHelpModals :: Interface action -> Action -> Transition action Model ()
updateHelpModals _ = \case
  GoTo pg -> pageModel .= pg

viewHelpModal :: Interface action -> ModalPage -> View action
viewHelpModal iface = \case
  MainPage              -> mainPageModal iface
  IntroArchitecturePage -> introModal IntroArchitecturePage iface
  IntroRegMemPage       -> introModal IntroRegMemPage iface
  IntroProgPage         -> introModal IntroProgPage iface
  IntroExecutionPage    -> introModal IntroExecutionPage iface
  GuideFilePage         -> guideModal GuideFilePage iface
  GuideExecPage         -> guideModal GuideExecPage iface
  AvailableInstructions -> instructionList iface

mainPageModal :: Interface action -> View action
mainPageModal iface =
  div_ [ class_ "modal is-active" ]
  [ div_ [class_ "modal-background" ] []
  , div_ [class_ "modal-content"]
      [ div_ [class_ "card"]
          [ header_ [class_ "card-header"]
              [ p_ [class_ "card-header-title"] [h1_ [class_ "title"] [ text "Welcome to the LegSIM Info Guide!"]]]
          , div_ [class_ "card-content"]
              [ div_ [class_ "block"]
                  [ h2_ [class_ "subtitle"] [text "To begin, select one of these guides:"]]
              , div_ [class_ "block"]
                  [ button_ [class_ "button is-large is-fullwidth mb-1", onClick $ passAction iface $ GoTo IntroArchitecturePage ] [text "Quick Introduction to Computer Architecture"]
                  , button_ [class_ "button is-large is-fullwidth mb-1", onClick $ passAction iface $ GoTo GuideFilePage] [text "Guide to Using this App"]
                  , button_ [class_ "button is-large is-fullwidth", onClick $ passAction iface $ GoTo AvailableInstructions] [text "List of Available Instructions"]
                  ]
              ]
          , footer_ [class_ "modal-card-foot"]
                    [button_ [class_ "button", onClick $ exitModal iface] [text "Close"]]
          ]
      ]
  , button_ [class_ "modal-close is-large", textProp "aria-label" "close", onClick $ exitModal iface] []
  ]

introModal :: ModalPage -> Interface action -> View action
introModal pg iface =
    div_ [ class_ "modal is-active" ]
    [ div_ [class_ "modal-background" ] []
    , div_ [class_ "modal-content"]
        [ div_ [class_ "card"]
            [ header_ [class_ "card-header"]
                [ p_ [class_ "card-header-title"] [h1_ [class_ "title"] [ text "Quick Introduction to Computer Architecture"]]]
            , div_ [class_ "card-content"]
                [ introPageBreadcrumb pg iface
                , content
                ]
            , footer_ [class_ "modal-card-foot"]
                      [button_ [class_ "button", onClick $ passAction iface $ GoTo MainPage] [text "Return to Help Page"]]
            ]
        ]
    , button_ [class_ "modal-close is-large", textProp "aria-label" "close", onClick $ exitModal iface] []
    ]
  where
    content = case pg of
        IntroArchitecturePage -> introContent
        IntroRegMemPage       -> registersContent
        IntroProgPage         -> programContent
        IntroExecutionPage    -> executionContent


introPageBreadcrumb :: ModalPage -> Interface action -> View action
introPageBreadcrumb pg iface =
  nav_ [class_ "breadcrumb has-arrow-separator", textProp "aria-label" "breadcrumbs"]
    [ ul_ []
      [ liCustom IntroArchitecturePage (pg == IntroArchitecturePage) "Introduction"
      , liCustom IntroRegMemPage (pg == IntroRegMemPage) "Registers and Memory"
      , liCustom IntroProgPage (pg == IntroProgPage) "Program"
      , liCustom IntroExecutionPage (pg == IntroExecutionPage) "Execution"
      ]
    ]
  where
    cur = textProp "aria-current" "page"
    liCustom mod isSelected desc =
      if isSelected
        then li_ [class_ "is-active"] [a_ [cur] [text desc]]
        else li_ [] [a_ [onClick $ passAction iface $ GoTo mod] [text desc]]

introContent :: View action
introContent =
  div_ [class_ "content"]
    [ h2_ [class_ "subtitle"] [text "Introduction"]
    , p_ [] [text "Welcome to LegSIM! Here, we shall discuss the basics of computer architecture. We seek to answer the question of \"What is a computer\" and \"How does it work?\" "]
    , p_ [] [text "At a fundamental level, a computer consists of data, which are stored in different places, and instruction to manipulate the data."]
    , h2_ [class_ "subtitle"] [text "Parts of a Computer"]
    , p_ [] [text "So what exactly are the components that make up the computer? In LegSIM, we focus on the 3 most important parts of the computer:"]
    , ul_ []
      [ li_ [] [text "Registers"]
      , li_ [] [text "Memory"]
      , li_ [] [text "Program"]
      ]
    , p_ [] [text "Move on to the next page to learn more about the registers and memory!"]
    ]

registersContent :: View action
registersContent =
  div_ [class_ "content"]
    [ h2_ [class_ "subtitle"] [text "Registers"]
    , p_ [] [text "At the top of the hierarchy, registers make up the core of the computer processor. Registers consists of a small amount of very fast storage (12 are available in LegSIM), and the data contained within them are manipulated using hardware instructions."]
    , p_ [] [text "While registers offer the fastest speed of execution, there is limited space available to store information. That is where the memory comes in."]
    , h2_ [class_ "subtitle"] [text "Memory"]
    , p_ [] [text "Also known as Random Access Memory (RAM), this is where the computer stores and loads data. While the memory has more storage space that registers, it is significantly slower."]
    , p_ [] [text "Now that we have the registers and memory, how do we actually tell a computer to do things? Lets take a look at how to do that in the next section!"]
    ]

programContent :: View action
programContent =
  div_ [class_ "content"]
    [ h2_ [class_ "subtitle"] [text "Program"]
    , p_ [] [text "A computer requires a sequence of instructions to be able to do things. Programs are just a series of instructions that a computer executes in sequence."]
    , p_ [] [text "An example of an instruction is the add instruction: "]
    , div_ [class_ "has-text-centered"] [ strong_ [class_ "is-family-monospace"] [text "ADD R2, R0, R1"]]
    , p_ [] [text "This instruction adds the contents of registers R0 and R1, and stores the result in register R2"]
    , p_ [] [text "Other than arithmetic instructions, there are load and store instructions in LegSIM that has the functionality to load from and store to memory. For example:"]
    , div_ [class_ "has-text-centered"] [ strong_ [class_ "is-family-monospace"] [text "LDUR R0, [R0, 5]"]]
    , p_ [] [text "This instruction loads the contents in memory location (value in R0 + offset 5) into register R0"]
    ]

executionContent :: View action
executionContent =
  div_ [class_ "content"]
    [ h2_ [class_ "subtitle"] [text "Execution"]
    , p_ [] [text "Putting everything we've learnt together, we can write a simple program that adds the same number twice and stores the result in memory:"]
    , div_ [class_ "has-text-centered"] [ strong_ [class_ "is-family-monospace"] [text "LDUR R0, [R0, 0]"], text " -- (1)"]
    , div_ [class_ "has-text-centered"] [ strong_ [class_ "is-family-monospace"] [text "ADD R2, R0, R0"], text " -- (2)"]
    , div_ [class_ "has-text-centered"] [ strong_ [class_ "is-family-monospace"] [text "STUR R3, [R3, 0]"], text " -- (3)"]
    , p_ [] [text "This program only contains 3 instructions, which are explained as follows:"]
    , ol_ [type_ "1"]
      [ li_ [] [text "Loads the memory content from location (R0 + offset 0), and puts that in register R0"]
      , li_ [] [text "Adds the data at R0 twice, stores the result in register R2"]
      , li_ [] [text "Stores the result of the addition in memory location (R3 + offset 0)"]
      ]
    , p_ [] [text "That concludes the quick introduction! For starters, try entering this program in the LegSIM editor. For more information, check out the guide to using this App in the info guide."]
    ]

guideModal :: ModalPage -> Interface action -> View action
guideModal pg iface =
    div_ [ class_ "modal is-active" ]
    [ div_ [class_ "modal-background" ] []
    , div_ [class_ "modal-content"]
        [ div_ [class_ "card"]
            [ header_ [class_ "card-header"]
                [ p_ [class_ "card-header-title"] [h1_ [class_ "title"] [ text "Guide to Using this App"]]]
            , div_ [class_ "card-content"]
                [ guidePageBreadcrumb pg iface
                , content
                ]
            , footer_ [class_ "modal-card-foot"]
                      [button_ [class_ "button", onClick $ passAction iface $ GoTo MainPage] [text "Return to Help Page"]]
            ]
        ]
    , button_ [class_ "modal-close is-large", textProp "aria-label" "close", onClick $ exitModal iface] []
    ]
  where
    content = case pg of
        GuideFilePage -> guideFileContent
        GuideExecPage -> guideSimContent

guidePageBreadcrumb :: ModalPage -> Interface action -> View action
guidePageBreadcrumb pg iface =
  nav_ [class_ "breadcrumb has-arrow-separator", textProp "aria-label" "breadcrumbs"]
    [ ul_ []
      [ liCustom GuideFilePage (pg == GuideFilePage) "Construction"
      , liCustom GuideExecPage (pg == GuideExecPage) "Simulation"
      ]
    ]
  where
    cur = textProp "aria-current" "page"
    liCustom mod isSelected desc =
      if isSelected
        then li_ [class_ "is-active"] [a_ [cur] [text desc]]
        else li_ [] [a_ [onClick $ passAction iface $ GoTo mod] [text desc]]

guideFileContent :: View action
guideFileContent =
  div_ [class_ "content"]
    [ figure_ [class_ "image-is-4by3"] [img_ [src_ "assets/fileCapture.png"]]
    , ol_ [type_ "1"]
      [ li_ [] [text "Create a new file"]
      , li_ [] [text "Write your program here"]
      , li_ [] [text "Look at the initial register and memory states"]
      , li_ [] [text "Create a new preset memory slot"]
      , li_ [] [text "Edit or delete memory slots"]
      , li_ [] [text "Compile and simulate the program"]
      ]
    ]

guideSimContent :: View action
guideSimContent =
  div_ [class_ "content"]
    [ figure_ [class_ "image-is-4by3"] [img_ [src_ "assets/simCapture.png"]]
    , ol_ [type_ "1"]
      [ li_ [] [text "Step through the program or reset the program to the start"]
      , li_ [] [text "Look at the current instruction being executed"]
      , li_ [] [text "Observe the state of the registers and memory"]
      , li_ [] [text "Switch to Construct mode to edit the program again"]
      ]
    ]

instructionList :: Interface action -> View action
instructionList iface =
  div_ [ class_ "modal is-active" ]
    [ div_ [class_ "modal-background" ] []
    , div_ [class_ "modal-content"]
        [ div_ [class_ "card"]
            [ header_ [class_ "card-header"]
                [ p_ [class_ "card-header-title"] [h1_ [class_ "title"] [ text "Instruction List"]]]
            , instrGuide
            ]
            , footer_ [class_ "modal-card-foot"]
                      [button_ [class_ "button", onClick $ passAction iface $ GoTo MainPage] [text "Return to Help Page"]
            ]
        ]
    , button_ [class_ "modal-close is-large", textProp "aria-label" "close", onClick $ exitModal iface] []
    ]

instrGuide :: View action
instrGuide =
  div_ [class_ "content"]
    [ ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "ADD R0, R1, R2"]
        , p_ [] [text "Adds the content of two registers and stores the result in a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 + R2"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "SUB R0, R1, R2"]
        , p_ [] [text "Subtracts the content in R1 by R2 and stores the result in a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 - R2"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "ADDI R0, R1, imm"]
        , p_ [] [text "Adds the content in R1 by an immediate and stores the result in a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 + imm"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "SUBI R0, R1, imm"]
        , p_ [] [text "Subtracts the content in R1 by an immediate and stores the result in a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 - imm"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "MUL R0, R1, imm"]
        , p_ [] [text "Multiplies the content of two registers and stores the result in a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 * R2"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "UDIV R0, R1, imm"]
        , p_ [] [text "Divides the content of R1 by R2 and stores the result in a register. Note the division treats the contents as unsigned integers"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 / R2"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "AND R0, R1, imm"]
        , p_ [] [text "Bitwise AND of two registers and stores the result in a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 & R2"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "ORR R0, R1, imm"]
        , p_ [] [text "Bitwise inclusive OR of two registers and stores the result in a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 | R2"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "EOR R0, R1, imm"]
        , p_ [] [text "Bitwise exclusive OR of two registers and stores the result in a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = R1 ⊕ R2"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "LDUR R0, [R1, offset]"]
        , p_ [] [text "Load the contents at the location (R1 + offset) into a register"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "R0 = mem[R1 + offset]"]]
        ]
      ]
    , ul_ []
      [ li_ [class_ "card-content"]
        [ strong_ [class_ "is-family-monospace"] [text "STUR R0, [R1, offset]"]
        , p_ [] [text "Store the contents of register R0 at memory location (R1 + offset)"]
        , p_ [] [text "Meaning: ", strong_ [class_ "is-family-monospace"] [text "mem[R1 + offset] = R0"]]
        ]
      ]
    ]

