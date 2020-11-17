{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module CPU where

import           Control.Lens
import           Data.Bits          (xor, (.&.), (.|.))
import qualified Data.IntMap.Strict as IM
import           Data.Word
import           Instructions
import qualified Memory             as Mem
import           Register

data CPU
    = CPU { _registers :: IM.IntMap Register.Model, _memory :: Mem.Memory }
    deriving ( Show, Eq )

makeLenses ''CPU

step :: Instruction -> CPU -> CPU
step instr initCPU = (resetSelectedRegisters . resetSelectedMemory) initCPU & \cpu -> case instr of
    ADD (Reg dest) (Reg src1) (Reg src2)
        -> cpu & regDataLens dest .~ regVal src1 + regVal src2
               & foldComp makeRegSelectedCPU [dest, src1, src2]
    SUB (Reg dest) (Reg src1) (Reg src2)
        -> cpu & regDataLens dest .~ regVal src1 - regVal src2
               & foldComp makeRegSelectedCPU [dest, src1, src2]
    ADDI (Reg dest) (Reg src) imm ->
        cpu & regDataLens dest .~ regVal src + imm
            & foldComp makeRegSelectedCPU [dest, src]
    SUBI (Reg dest) (Reg src) imm ->
        cpu & regDataLens dest .~ regVal src - imm
            & foldComp makeRegSelectedCPU [dest, src]
    MUL (Reg dest) (Reg src1) (Reg src2)
        -> cpu & regDataLens dest .~ regVal src1 * regVal src2
               & foldComp makeRegSelectedCPU [dest, src1, src2]
    UDIV (Reg dest) (Reg src1) (Reg src2)
        -> cpu & regDataLens dest .~ regVal src1 `div` regVal src2
               & foldComp makeRegSelectedCPU [dest, src1, src2]
    LDUR (Reg dest) (Reg src) imm ->
        cpu & regDataLens dest .~ memVal (fromIntegral $ regVal src + imm)
            & foldComp makeRegSelectedCPU [dest, src]
            & makeMemSelectedCPU (fromIntegral $ regVal src + imm)
    STUR (Reg src) (Reg dest) imm ->
        cpu & memDataLens (fromIntegral $ regVal dest + imm) .~ regVal src
            & foldComp makeRegSelectedCPU [dest, src]
            & makeMemSelectedCPU (fromIntegral $ regVal dest + imm)
    AND (Reg dest) (Reg src1) (Reg src2)
        -> cpu & regDataLens dest .~ regVal src1 .&. regVal src2
               & foldComp makeRegSelectedCPU [dest, src1, src2]
    ORR (Reg dest) (Reg src1) (Reg src2)
        -> cpu & regDataLens dest .~ regVal src1 .|. regVal src2
               & foldComp makeRegSelectedCPU [dest, src1, src2]
    EOR (Reg dest) (Reg src1) (Reg src2)
        -> cpu & regDataLens dest .~ regVal src1 `xor` regVal src2
               & foldComp makeRegSelectedCPU [dest, src1, src2]
    IDENTITY -> cpu
  where
    regVal :: Int -> Word64
    regVal idx = initCPU ^. registers . at idx . non (Register.initialModel 0) . registerData

    regDataLens :: Int -> Lens' CPU Word64
    regDataLens dest = registers . at dest . non (Register.initialModel 0) . registerData

    memVal :: Int -> Word64
    memVal idx = initCPU ^. memory . Mem.memorySlot . at idx . non Mem.initialMemorySlot . Mem.memoryData

    memDataLens :: Int -> Lens' CPU Word64
    memDataLens dest = memory . Mem.memorySlot . at dest . non Mem.initialMemorySlot . Mem.memoryData

resetSelectedRegisters :: CPU -> CPU
resetSelectedRegisters cpu =
  cpu & registers . each %~ makeNotSelected

resetSelectedMemory :: CPU -> CPU
resetSelectedMemory cpu =
    cpu & memory . Mem.memorySlot . each %~ makeNotSelected

makeRegSelectedCPU :: Int -> CPU -> CPU
makeRegSelectedCPU reg cpu =
  cpu & registers . at reg . non (initialModel 0) %~ makeSelected

makeMemSelectedCPU :: Int -> CPU -> CPU
makeMemSelectedCPU memloc cpu =
  cpu & memory . Mem.memorySlot . at memloc . non Mem.initialMemorySlot %~ makeSelected

foldComp f xs b = foldr f b xs
