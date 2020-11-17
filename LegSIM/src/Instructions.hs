{-# LANGUAGE GADTs              #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE StandaloneDeriving #-}

module Instructions where

import           Control.Lens
import           Data.List    (intersperse)
import           Data.Word
import           Helpers
import           Miso
import qualified Miso.String  as MS

data IsExecuted = Executed | NotExecuted
    deriving ( Eq, Show )

type PCounter = Int

data Reg where
    Reg :: Int -> Reg

instance Show Reg where
    show (Reg n) = 'R' : show n

data Instruction where
    ADD :: Reg -> Reg -> Reg -> Instruction
    SUB :: Reg -> Reg -> Reg -> Instruction
    ADDI :: Reg -> Reg -> Word64 -> Instruction
    SUBI :: Reg -> Reg -> Word64 -> Instruction
    MUL :: Reg -> Reg -> Reg -> Instruction
    UDIV :: Reg -> Reg -> Reg -> Instruction
    LDUR :: Reg -> Reg -> Word64 -> Instruction
    STUR :: Reg -> Reg -> Word64 -> Instruction
    AND :: Reg -> Reg -> Reg -> Instruction
    ORR :: Reg -> Reg -> Reg -> Instruction
    EOR :: Reg -> Reg -> Reg -> Instruction
    IDENTITY :: Instruction -- Note that this is not meant to be exposed!

deriving instance Eq Reg
deriving instance Eq Instruction

class RenderMiso d where
    renderMiso :: d -> View action

instance RenderMiso Instruction where
    renderMiso (ADD r1 r2 r3) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "ADD " ]
        , text (showm r1 <./> showm r2 <./> showm r3)
        ]
    renderMiso (SUB r1 r2 r3) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "SUB " ]
        , text (showm r1 <./> showm r2 <./> showm r3)
        ]
    renderMiso (ADDI r1 r2 imm) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "ADDI " ]
        , text (showm r1 <./> showm r2 <./> showm imm)
        ]
    renderMiso (SUBI r1 r2 imm) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "SUBI " ]
        , text (showm r1 <./> showm r2 <./> showm imm)
        ]
    renderMiso (MUL r1 r2 r3) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "MUL " ]
        , text (showm r1 <./> showm r2 <./> showm r3)
        ]
    renderMiso (UDIV r1 r2 r3) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "UDIV " ]
        , text (showm r1 <./> showm r2 <./> showm r3)
        ]
    renderMiso (LDUR r1 r2 imm) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "LDUR " ]
        , text (showm r1 <./> "[" <> showm r2 <./> showm imm <> "]")
        ]
    renderMiso (STUR r1 r2 imm) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "STUR " ]
        , text (showm r1 <./> "[" <> showm r2 <./> showm imm <> "]")
        ]
    renderMiso (AND r1 r2 r3) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "AND " ]
        , text (showm r1 <./> showm r2 <./> showm r3)
        ]
    renderMiso (ORR r1 r2 r3) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "ORR " ]
        , text (showm r1 <./> showm r2 <./> showm r3)
        ]
    renderMiso (EOR r1 r2 r3) = span_ [ class_ "is-family-monospace" ]
        [ strong_ [] [ text "EOR " ]
        , text (showm r1 <./> showm r2 <./> showm r3)
        ]

instance Show Instruction where
    show (ADD r1 r2 r3) = "ADD " ++ show r1 <./> show r2 <./> show r3
    show (SUB r1 r2 r3) = "SUB " ++ show r1 <./> show r2 <./> show r3

type Program = [ Instruction ]

renderProgram :: PCounter -> Program -> [ View action ]
renderProgram pc prog = prog & itraversed
    %@~ (\i instr -> renderInstr i (if i == pc then Executed else NotExecuted)
         instr)

renderInstr :: Int -> IsExecuted -> Instruction -> View action
renderInstr lineNum isExe instr = p_ bg [ strong_ [] [text $ showm lineNum], text " ", renderMiso instr ]
  where
    bg = case isExe of
        Executed    -> [ class_ "has-background-warning" ]
        NotExecuted -> []

sampleProgram :: Program
sampleProgram = [ ADD (Reg 0) (Reg 1) (Reg 2)
                , SUB (Reg 2) (Reg 0) (Reg 0)
                , ADD (Reg 3) (Reg 0) (Reg 1)
                ]

renderSampleProgram :: [ View action ]
renderSampleProgram = renderProgram 0 sampleProgram
