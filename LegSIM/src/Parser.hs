{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Control.Lens
import           Control.Monad
import qualified Data.List.NonEmpty         as NE
import           Data.Text                  (Text)
import           Data.Validation
import           Data.Void
import           Data.Word
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Instructions

type Parser = Parsec Void Text

parseProgram :: [Text] -> Validation [String] [Instruction]
parseProgram prog =
    prog ^@.. itraversed . ifiltered (\i a -> a /= "")
      & traverse (\(i, instr) -> parse pInstrs "" instr & liftError (extractErr i . bundleErrors))
  where
    extractErr idx (err NE.:| _) = pure $ "In line " <> show idx <> ", " <> parseErrorPretty err
sc :: Parser ()
sc = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

int :: Parser Int
int = lexeme L.decimal

comma :: Parser ()
comma = void (symbol ",") <?> "- Expecting a comma"

parseReg :: Parser Reg
parseReg = do
  regLab <- symbol "R"
  regNum <- int
  when (regNum > 12) $ fail "Register number greater than 12"
  return (Reg regNum)

parseRInstr :: Text -> (Reg -> Reg -> Reg -> Instruction) -> Parser Instruction
parseRInstr instrName instr = do
  void (symbol instrName)
  dest <- parseReg
  comma
  src1 <- parseReg
  comma
  src2 <- parseReg
  return $ instr dest src1 src2

parseIInstr :: Text -> (Reg -> Reg -> Word64 -> Instruction) -> Parser Instruction
parseIInstr instrName instr = do
  void (symbol instrName)
  dest <- parseReg
  comma
  src  <- parseReg
  comma
  imm  <- int
  return $ instr dest src (fromIntegral imm)

parseLInstr :: Text -> (Reg -> Reg -> Word64 -> Instruction) -> Parser Instruction
parseLInstr instrName instr = do
  void (symbol instrName)
  dest <- parseReg
  comma
  void (symbol "[")
  src  <- parseReg
  comma
  imm  <- int
  void (symbol "]")
  return $ instr dest src (fromIntegral imm)

pRInstr =
  zipWith parseRInstr
  ["ADD", "SUB", "MUL", "UDIV", "AND", "ORR", "EOR"]
  [ADD, SUB, MUL, UDIV, AND, ORR, EOR]

pIInstr =
  zipWith parseIInstr
  ["ADDI", "SUBI"]
  [ADDI, SUBI]

pLInstr =
  zipWith parseLInstr
  ["LDUR", "STUR"]
  [LDUR, STUR]

pInstrs = choice (pRInstr <> pIInstr <> pLInstr) <?> "an instruction"
