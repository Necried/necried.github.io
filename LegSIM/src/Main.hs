-- | Haskell language pragma
{-# LANGUAGE CPP               #-}
{-# LANGUAGE JavaScriptFFI     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}

-- | Haskell module declaration
module Main where

import           CPU
import           CPUComponent
import           HelpModals                       (ModalPage (..),
                                                   viewHelpModal)
import qualified HelpModals
import           Helpers
import           Instructions
import qualified Memory                           as Mem
import qualified Register
-- | Miso framework import
import           Control.Lens
import qualified Data.ByteString.Lazy             as B
import qualified Data.IntMap.Strict               as IM
import qualified Data.Text                        as T
import           Data.Word
import           Miso                             hiding (at)
import           Miso.String                      (MisoString, fromMisoString,
                                                   toMisoString)
import           Text.Read
-- | JSAddle import
#ifndef __GHCJS__
import           Language.Javascript.JSaddle.Warp as JSaddle hiding (prop)
import qualified Network.Wai.Handler.Warp         as Warp
import           Network.WebSockets
#endif
import           Control.Monad.IO.Class
-- import           GHCJS.Types
-- TODO: remove this when done testing I think
import qualified Data.List.NonEmpty               as NE
import           Data.Validation
import           Parser
import           Text.Megaparsec

-- | Type synonym for an application model
data Model = Model
      { _cpu                :: CPU
      , _page               :: Page
      , _fileInfo           :: FileInfo
      , _programState       :: Either [MisoString] Program
      , _constructViewState :: ConstructViewState
      , _modal              :: Maybe ModalTy
      , _pc                 :: Int
      , _simulationCPU      :: Maybe CPU
      }
    deriving ( Show, Eq )

data Page = ConstructPage | SimulatePage
    deriving ( Show, Eq, Enum )

data FileInfo
    = FileInfo { _nextId :: Int, _selected :: Int, _files :: IM.IntMap File }
    deriving ( Show, Eq )

data File = File { _fileName :: MisoString, _fileData :: MisoString }
    deriving ( Show, Eq )

data ConstructViewState = RegisterView | MemoryView deriving (Eq, Show)

type ModalWarningMsg = Maybe MisoString

data ModalTy =
      MemModal MisoString MisoString ModalWarningMsg
      | MemModifyModal Int MisoString ModalWarningMsg
      | HelpModal ModalPage
      deriving (Eq, Show)

data MemModalAction =
      EnterSlotInfo MisoString
    | EnterDataInfo MisoString
    | ConfirmInput
    | CancelInput
    deriving (Show, Eq)

data MemModifyModalAction =
      EnterModifyDataInfo MisoString
    | ConfirmModifyInput
    | CancelModifyInput

makeLenses ''Model

makeLenses ''FileInfo

makeLenses ''File

-- | Sum type for application events
data Action =
      NoOp
      | InitialAction
      | RegisterEnterData (MisoString -> Register.Action) MisoString
      | NewFile
      | OpenFile Int
      | FileContentsChanged Int MisoString
      | GoToSimulate
      | GoToConstruct
      | ViewRegisterData
      | ViewMemoryData
      | IncrementPC
      | ResetPC
      | MemAction Mem.Action
      | MemDeleteSlot Int
      | MemModalAction MemModalAction
      | MemModifyModalAction MemModifyModalAction
      | OpenModal ModalTy
      | MemEnterData Int Word64
      | DeleteErr
      | ExitHelpModal
      | HelpModalAction HelpModals.Action


#ifndef __GHCJS__
runApp :: JSM () -> IO ()
runApp f = do
    codeMirrorJS <- B.readFile "./codemirror-5.58.1/lib/codemirror.js"
    codeMirrorMode
        <- B.readFile "./codemirror-5.58.1/mode/javascript/javascript.js"
    jSaddle <- JSaddle.jsaddleOr defaultConnectionOptions (f >> syncPoint)
        (JSaddle.jsaddleAppWithJs (B.append (JSaddle.jsaddleJs False)
                                   $ codeMirrorJS <> "\n" <> codeMirrorMode))
    Warp.runSettings
        (Warp.setPort 8081 (Warp.setTimeout 3600 Warp.defaultSettings)) jSaddle

#else
runApp :: IO () -> IO ()
runApp app = app
#endif

-- | Entry point for a miso application
main :: IO ()
main = runApp $ startApp $ App { initialAction
    = NoOp -- initial action to be executed on application load
    , model = Model
        { _cpu = initialCPUModel
        , _page = ConstructPage
        , _fileInfo = initialFileInfo
        , _programState = Right []
        , _constructViewState = RegisterView
        , _modal = Nothing
        , _pc = 0
        , _simulationCPU = Nothing
        } -- initial model
    , update = fromTransition . updateModel      -- update function
    , view = viewModel            -- view function
    , events = defaultEvents        -- default delegated events
    , subs = []                   -- empty subscription list
    , mountPoint = Nothing          -- mount point for application (Nothing defaults to 'body')
    , logLevel = Off                -- used during prerendering to see if the VDOM and DOM are in sync (only used with `miso` function)
    }

initialCPUModel :: CPU
initialCPUModel = CPU
    (IM.fromList [ ( i, Register.initialModel i ) | i <- [ 0 .. 12 ] ]) Mem.initialMemory

initialFileInfo :: FileInfo
initialFileInfo = FileInfo { .. }
  where
    _nextId = 2

    _selected = 1

    _files = IM.singleton 1 $ File { _fileName = "File1", _fileData = "" }


-- | Updates model, optionally introduces side effects
updateModel :: Action -> Transition Action Model ()
updateModel action = case action of
  GoToSimulate -> do
    curFileInfo <- use fileInfo
    let parseResult = parseProgram (T.splitOn "\n" $ fromMisoString $ curFileInfo ^. files . at (curFileInfo ^. selected) . non (File "" "") . fileData)
    case parseResult of
      Failure errs -> programState .= Left (map toMisoString errs)
      Success instrs -> do
        pc .= -1
        cpu . memory . Mem.inputAllowed .= Mem.InputAllowed False
        curCPU <- use cpu
        programState .= Right instrs
        page .= SimulatePage
        simulationCPU .= Just curCPU
        -- Miso.scheduleIO_ $ Miso.consoleLog (showm curCPU)
  GoToConstruct -> do
    storedCPU <- use simulationCPU
    case storedCPU of
      Just stored -> cpu .= stored
      Nothing     -> pure ()
    simulationCPU .= Nothing
    cpu . memory . Mem.inputAllowed .= Mem.InputAllowed True
    page .= ConstructPage
  NewFile -> do
    curFileInfo <- use fileInfo
    fileInfo . files %= IM.insert (curFileInfo ^. nextId) (File ("File" <> showm (curFileInfo ^. nextId)) "")
    fileInfo . selected .= curFileInfo ^. nextId
    fileInfo . nextId += 1
  OpenFile fileIdx -> do
    fileInfo . selected .= fileIdx
  FileContentsChanged fileIdx contents -> do
    curFileInfo <- use fileInfo
    let f = curFileInfo ^. files . at fileIdx . non (File "" "")
    fileInfo . files %= IM.insert fileIdx (File (f ^. fileName) contents)
  ViewRegisterData ->
    constructViewState .= RegisterView
  ViewMemoryData ->
    constructViewState .= MemoryView
  DeleteErr -> programState .= Right []
  OpenModal modalTy -> modal .= Just modalTy
  MemModalAction mAction -> updateMemModalAction mAction
  HelpModalAction (HelpModals.GoTo pg) -> do
    mModal <- use modal
    case mModal of
      Nothing            -> pure ()
      Just (HelpModal _) -> modal .= Just (HelpModal pg)
  MemModifyModalAction mAction -> updateMemModifyModalAction mAction
  MemEnterData idx num -> cpu . memory . Mem.memorySlot %= IM.insert idx (Mem.MemorySlot num NotSelected)
  MemDeleteSlot idx -> cpu . memory . Mem.memorySlot %= IM.delete idx
  MemAction action -> zoom (cpu . memory) $ Mem.updateMemory iMemory action
  IncrementPC -> do
    pc += 1
    simCPU <- use cpu
    eProg <- use programState
    newPc <- use pc
    case eProg of
      Right prog -> cpu .= CPU.step (prog ^. pre (ix newPc) . non IDENTITY) simCPU
      _ -> pure ()
  ResetPC -> do
    pc .= -1
    prevCPU <- use simulationCPU
    case prevCPU of
      Nothing    -> pure ()
      Just myCPU -> cpu .= myCPU
  ExitHelpModal -> modal .= Nothing
  _ -> pure ()

updateMemModalAction :: MemModalAction -> Transition Action Model ()
updateMemModalAction mAction = do
      curModal <- use modal
      case curModal of
        Just (MemModal s0 d0 w) ->
          case mAction of
            EnterSlotInfo s -> modal .= Just (MemModal s d0 w)
            EnterDataInfo d -> modal .= Just (MemModal s0 d w)
            ConfirmInput ->
              let sStr = fromMisoString s0
                  dStr = fromMisoString d0
                  mParsedData = (,) <$> readMaybe @Int sStr <*> readMaybe @Word64 dStr
              in case mParsedData of
                Just (sInt, dWord) -> do
                  modal .= Nothing
                  toTransition $ \m -> m <#
                    pure (MemEnterData sInt dWord)
                _ -> modal .= Just (MemModal s0 d0 $ Just "Invalid data - only positive numbers are accepted!")
            CancelInput ->
                  modal .= Nothing

updateMemModifyModalAction :: MemModifyModalAction -> Transition Action Model ()
updateMemModifyModalAction mAction = do
      curModal <- use modal
      case curModal of
        Just (MemModifyModal s0 d0 w) ->
          case mAction of
            EnterModifyDataInfo d -> modal .= Just (MemModifyModal s0 d w)
            ConfirmModifyInput ->
              let dStr = fromMisoString d0
                  mParsedData = readMaybe @Word64 dStr
              in case mParsedData of
                   Just dWord -> do
                     modal .= Nothing
                     toTransition $ \m -> m <#
                       pure (MemEnterData s0 dWord)
                   _ ->
                     modal .= Just (MemModifyModal s0 d0 $ Just "Invalid data - only positive numbers are accepted!")
            CancelModifyInput ->
              modal .= Nothing
{-
codemirrorSetup :: View Action
codemirrorSetup =
  div_ []
    [ script_ [ src_ "codemirror-5.58.1/lib/codemirror.js" ] ""
    , link_ [ rel_ "stylesheet", href_ "codemirror-5.58.1/lib/codemirror.css" ]
    , script_ [src_ "codemirror-5.58.1/mode/javascript/javascript.js" ] ""
    ]
-}
-- | Constructs a virtual DOM from a model
viewModel :: Model -> View Action
viewModel m@Model {} = div_ [ class_ "has-background-white-ter" ]
    [ link_ [ rel_ "stylesheet", href_ bulmaUrl ]
    , link_ [ rel_ "stylesheet", href_ faUrl ]
      -- , codemirrorSetup
    , mModal
    , myNavbar m
    , curPage
    ]
  where
    curPage = case m ^. page of
        ConstructPage -> constructLayout m
        SimulatePage  -> simulateLayout m

    mModal = case m ^. modal of
        Nothing                        -> span_ [] []
        Just (MemModal slot d w)       -> memSlotModal slot d w
        Just (MemModifyModal slot d w) -> memModifyModal slot d w
        Just (HelpModal pg)            -> viewHelpModal iHelpModal pg

bulmaUrl :: MisoString
bulmaUrl = "css/mystyles.css"
  -- "https://cdn.jsdelivr.net/npm/bulma@0.9.1/css/bulma.min.css"

faUrl :: MisoString
faUrl = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"

myNavbar :: Model -> View Action
myNavbar m = nav_ [ class_ "navbar"
                  , textProp "role" "navigation" :: Attribute Action
                  , textProp "aria-label" "main navigation"
                  ]
    [ div_ [ class_ "navbar-brand" ]
          [ div_ [ class_ "navbar-item has-text-weight-bold is-family-monospace"
                 ] [ text "LegSIM" ]
          ]
    , navbarStart
    , navbarEnd
    ]
  where
    navbarStart = div_ [ class_ "navbar-start" ] [ navbarTabs ]

    navbarTabs = mkTabs (fromEnum $ m ^. page)
        [ ( "Construct", GoToConstruct ), ( "Simulate", GoToSimulate ) ]

    navbarEnd = div_ [ class_ "navbar-end" ]
        [ div_ [ class_ "navbar-item" ]
              [ div_ [ class_ "buttons" ]
                      [ button_ [ class_ "button is-info", onClick $ OpenModal $ HelpModal MainPage  ]  [strong_ [] [ text "Info" ] ] ]
              ]
        ]


constructLayout :: Model -> View Action
constructLayout m = div_ []
    [ div_ [ class_ "box" ] condErrMsg
    , div_ [ class_ "block tile is-ancestor" ]
          [ div_ [ class_ "tile is-8 is-parent" ]
                [ div_ [ class_ "tile is-child box" ]
                      [ mkNavbar (Navbar Nothing (Just [ filesTabs ])
                                  (Just [ addFile ]))
                      , textarea_
                            [ id_ "myCodeMirror"
                            , boolProp "spellcheck" False
                            , class_
                                  "textarea has-fixed-size is-family-monospace"
                            , rows_ "15"
                            , placeholder_ "Enter your program here"
                            , value_ (m ^. fileInfo . files . at
                                      (m ^. fileInfo . selected) . non
                                      (File "" "") . fileData)
                            , onInput $ FileContentsChanged
                                  (m ^. fileInfo . selected)
                            ] []
                      ]
                ]
          , div_ [ class_ "tile is-parent" ]
                [ div_ [ class_ "title is-child box" ]
                      $ [ p_ [ class_ "title" ] [ cpuTabs ] ] ++ constructView
                ]
          ]
    ]
  where
    cpuTabs :: View Action
    cpuTabs = mkTabs active [ ( "Registers", ViewRegisterData ), ( "Memory", ViewMemoryData ) ]
      where
        active = if m ^. constructViewState == RegisterView then 0 else 1

    filesTabs :: View Action
    filesTabs = mkTabs (m ^. fileInfo . selected - 1) $ fileTabs
      where
        fileTabs = fmap (\( idx, f ) -> ( f ^. fileName, OpenFile idx ))
            $ m ^. fileInfo . files ^@.. itraversed

    addFile :: View Action
    addFile = button_ [ class_ "button is-info", onClick NewFile ]
        [ mkIcon Nothing (Just IsLarge) TextWhite "fa fa-plus-square"
        , span_ [] [ text "New" ]
        ]

    condErrMsg :: [View Action]
    condErrMsg = case m ^. programState of
      Left err -> [errMsg err]
      Right _  -> []

    constructView :: [View Action]
    constructView = case m ^. constructViewState of
      RegisterView -> rs m
      MemoryView -> makeNewMemSlot : Mem.viewMemory iMemory (m ^. cpu . memory)

    makeNewMemSlot :: View Action
    makeNewMemSlot = button_ [ class_ "button is-info", onClick (OpenModal (MemModal "" "" Nothing)) ]
      [ mkIcon Nothing (Just IsLarge) TextWhite "fa fa-plus-square"
      , span_ [] [ text "New Memory Slot" ]
      ]

simulateLayout :: Model -> View Action
simulateLayout m = div_ []
    [ div_ [ class_ "box" ]
          [ h5_ [ class_ "title is-5" ] [ text "File1" ], simButtons m ]
    , div_ [ class_ "tile is-ancestor block" ]
          [ div_ [ class_ "tile is-8 is-parent is-vertical" ]
                [ div_ [ class_ "tile is-child box" ] $ renderProgram (m ^. pc) prog]
          , div_ [ class_ "tile is-parent" ]
                [ div_ [ class_ "title is-child box" ]
                      $ [ p_ [ class_ "title" ] [text "Registers" ]] ++ (rs m)
                ]
          , div_ [ class_ "tile is-parent" ]
              [ div_ [ class_ "title is-child box" ]
                      $ [ p_ [ class_ "title" ] [ text "Memory" ]] ++ (Mem.viewMemory iMemory (m ^. cpu . memory))
              ]
          ]
    ]
    where
      prog = case m ^. programState of
        Left _     -> []
        Right prog -> prog

simButtons :: Model -> View Action
simButtons m = div_ [ class_ "buttons" ]
    [ button_ [ class_ "button is-link", onClick IncrementPC ]
          [ mkIcon Nothing (Just IsLarge) TextWhite "fa fa-forward"
          , span_ [] [ text "Next" ]
          ]
    , button_ [ class_ "button is-danger", onClick ResetPC ]
          [ mkIcon Nothing (Just IsLarge) TextWhite "fa fa-undo"
          , span_ [] [ text "Reset" ]
          ]
    ]

rs :: Model -> [ View Action ]
rs m = m ^. cpu . registers ^.. folded . to (Register.viewModel iRegister)

errMsg :: [MisoString] -> View Action
errMsg errs = article_ [ class_ "message is-danger" ] $
    [ div_ [ class_ "message-header" ]
          [ p_ [] [ "Error" ]
          , button_ [ class_ "delete", textProp "aria-label" "delete", onClick DeleteErr ] []
          ]
    , div_ [ class_ "message-body"] $ map renderErr errs
    ]
    where
      renderErr err =
        div_ [] [text err]

memSlotModal :: MisoString -> MisoString -> ModalWarningMsg -> View Action
memSlotModal slot d mWarn =
    div_ [ class_ "modal is-active" ]
      [ div_ [class_ "modal-background" ] []
      , div_ [class_ "modal-content"]
          [ div_ [class_ "card"]
              [ header_ [class_ "card-header"]
                  [ p_ [class_ "card-header-title"] [text "New memory slot"]]
              , div_ [class_ "card-content"]
                  [ div_ [class_ "field"]
                        [ label_ [class_ "label"] [text "Slot"]
                        , div_ [class_ "control"] [input_ [class_ "input", type_ "text", value_ slot, onChange (MemModalAction . EnterSlotInfo) ]]
                        ]
                  , div_ [class_ "field"]
                        [ label_ [class_ "label"] [text "Data"]
                        , div_ [class_ "control"] [input_ [class_ "input", type_ "text", value_ d, onChange (MemModalAction . EnterDataInfo) ]]
                        ]
                  , warnMsg
                  ]
              , footer_ [class_ "modal-card-foot"]
                        [ button_ [class_ "button is-success", onClick (MemModalAction ConfirmInput) ] [text "Create Slot"]
                        , button_ [class_ "button is-danger", onClick (MemModalAction CancelInput) ] [text "Discard"]
                        ]
              ]
          ]
      , button_ [class_ "modal-close is-large", textProp "aria-label" "close", onClick (MemModalAction CancelInput)] []
      ]
  where
    warnMsg = case mWarn of
      Nothing      -> span_ [] []
      Just warning -> div_ [class_ "has-text-danger"] [text warning]

memModifyModal :: Int -> MisoString -> ModalWarningMsg -> View Action
memModifyModal slot d mWarn =
      div_ [ class_ "modal is-active" ]
      [ div_ [class_ "modal-background" ] []
      , div_ [class_ "modal-content"]
          [ div_ [class_ "card"]
              [ header_ [class_ "card-header"]
                  [ p_ [class_ "card-header-title"] [text $ "Modify memory slot " <> showm slot]]
              , div_ [class_ "card-content"]
                  [ div_ [class_ "field"]
                        [ label_ [class_ "label"] [text "Slot"]
                        , div_ [class_ "control"] [input_ [class_ "input", type_ "text", value_ d, onChange (MemModifyModalAction . EnterModifyDataInfo) ]]
                        ]
                  ]
              , warnMsg
              , footer_ [class_ "modal-card-foot"]
                        [ button_ [class_ "button is-success", onClick (MemModifyModalAction ConfirmModifyInput) ] [text "Modify Slot"]
                        , button_ [class_ "button is-danger", onClick (MemModifyModalAction CancelModifyInput) ] [text "Discard"]
                        ]
              ]
          ]
      , button_ [class_ "modal-close is-large", textProp "aria-label" "close", onClick (MemModifyModalAction CancelModifyInput)] []
      ]
      where
        warnMsg = case mWarn of
          Nothing      -> span_ [] []
          Just warning -> div_ [class_ "has-text-danger"] [text warning]



iRegister :: Register.Interface Action
iRegister = Register.Interface { typeAction = RegisterEnterData }

iMemory :: Mem.Interface Action
iMemory = Mem.Interface {
      Mem.passAction = MemAction
    , Mem.modifyData = \slot d -> OpenModal $ MemModifyModal slot d Nothing
    }

iHelpModal :: HelpModals.Interface Action
iHelpModal = HelpModals.Interface {
  HelpModals.passAction = HelpModalAction
  , HelpModals.exitModal = ExitHelpModal
  }

{-
foreign import javascript unsafe
  "var $r = CodeMirror.fromTextArea(document.getElementById('myTextArea'), {theme: 'dracula', mode: 'javascript', lineNumbers: true});"
  codeMirror :: IO JSVal
-}
