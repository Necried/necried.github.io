module Bidirectional exposing (..)

import Browser exposing(sandbox)

import List exposing (range)
import Parser exposing(..)

import Html
import Html.Attributes as A
import Html.Events as E
import Set exposing (Set)
import Dict exposing (Dict)
import Html.Attributes exposing (style)

-- Syntax of our simple language as an ADT
type Expr 
  = Var String 
  | If Expr {-condition-} Expr {-then-} Expr {-else-}
  | FunDecl String {-input variable-} Expr {-body-}
  | Integer Int
  | Boolean Bool
  | BinOp Op Expr Expr
  | PredOp PredOp Expr Expr
  | FunApp Expr Expr
  | Ann Expr Ty
  
type Op
  = Plus
  | Minus
  | Mult
  | Divide
  
type PredOp
  = LT
  | GT
  | LTE
  | GTE
  | Eq
  | And
  | Or

-- An ADT to represent types of our language
-- Examples
-- FunTy BooleanTy IntegerTy                   means  Bool -> Int
-- FunTy BooleanTy (FunTy IntegerTy IntegerTy) means  Bool -> (Int -> Int)
type Ty
  = IntegerTy
  | BooleanTy
  | FunTy Ty {- -> -} Ty

-- A context stores mappings from variable names to types
-- The context gets populated as the type inference algorithm traverses the program
type alias Ctx = Dict String Ty

-- The infer function of our bidirectional type inferencer.
-- Takes a context and expression to infer, and returns the type of the expression on success.
-- Fill in for BT-VAR and BT-APP with the following typing rules:
--              x : t ∈ Γ
--            ------------- T-VAR
--              Γ ⊢ x <= t
--
--          Γ ⊢ e1 => t1 -> t2        Γ ⊢ e2 <= t1
--        ------------------------------------------- BT-APP
--                    Γ ⊢ e1 e2 => t2
inferType : Ctx -> Expr -> Maybe Ty
inferType ctx expr =
  case expr of 
    Var x -> Dict.get x ctx
    Boolean _ -> Just BooleanTy
    Integer _ -> Just IntegerTy
    Ann e ty -> checkType ctx e ty
    FunApp e1 e2 ->
      case (inferType ctx e1) of
        Just (FunTy t1 t2) ->
          case (checkType ctx e2 t1) of
            Just _ -> Just t2
            Nothing -> Nothing
        _ -> Nothing
    _ -> Nothing -- Can't figure out types, should add type annotations

-- The check function of our bidirectional type inferencer.
-- Takes a context, an expression and its type
--   and returns the type of the expression on success, which matches the input type given.
-- Fill in for BT-ABS with the following typing rule:
--            Γ, x : t1 ⊢ e <= t2
--      ----------------------------- BT-ABS
--         Γ ⊢ \x -> e <= t1 -> t2
checkType : Ctx -> Expr -> Ty -> Maybe Ty
checkType ctx expr ty = 
  case expr of
    BinOp op e1 e2 ->
      case (checkType ctx e1 ty, checkType ctx e2 ty) of
        (Just t1, Just t2) -> Just ty
        _ -> Nothing
    PredOp op e1 e2 ->
      let
          typ = if List.member op [LT,GT,LTE,GTE] then IntegerTy
                else BooleanTy
      in
        case (inferType ctx e1, inferType ctx e2) of
          (Just t1, Just t2) ->
            if t1 == t2
              then Just BooleanTy
              else Nothing
          _ -> Nothing
    FunDecl x e ->
      case ty of
        FunTy t1 t2 ->
          case checkType (ctx |> Dict.insert x t1) e t2 of
            Just tt2 -> Just ty 
            _ -> Nothing
        _ -> Nothing
    If e1 e2 e3 ->
      case (checkType ctx e1 BooleanTy, checkType ctx e2 ty, checkType ctx e3 ty) of
        (Just BooleanTy, Just ty2, Just ty3) -> Just ty
        _ -> Nothing
    _ -> case inferType ctx expr of
      Just ty1 -> if ty == ty1
                  then Just ty
                  else Nothing
      Nothing -> Nothing

-- All the inference rules for the other language constructs are given below here
-- for completeness:
{-

 ------------------------------- (BT-BOOLEAN-TY)
  Γ ⊢ Boolean _ => BooleanTy
  

 ------------------------------- (BT-INT-TY)
  Γ ⊢ Integer _ => IntegerTy
  
  
  Γ ⊢ e1 <= BooleanTy   Γ ⊢ e2 <= τ    Γ ⊢ e3 <= τ 
------------------------------------------------------- (T-IF)
            Γ |- if e1 then e2 else e3 <= τ
            
    Γ ⊢ e1 <= τ    Γ ⊢ e2 <= τ 
--------------------------------------- (BT-BINOP)
      Γ ⊢ BinOp Op e1 e2 <= τ
      
    Γ ⊢ e1 => τ1    Γ ⊢ e2 => τ2    τ1 == τ2   
----------------------------------------------- (BT-PREDOP)
        Γ ⊢ PredOp Op e1 e2 <= BooleanTy
-}


-- PARSER
-- Everything beneath here defines the parser for the simple language.
-- You can change the initial test input by changing `myTestText`

-- Your initial test text
myTestText = initTest

initTest = "(\\x -> x) : Int -> Int"
test0 = "-84"
test1 = "(if (-5 < -10) then 2 else 1) : Int"
test2 = "(\\x -> if (4 > 40) then (14 * 5) else (8//2)) : Int -> Int"
test3 = "False"
test4 = "(\\y -> \\x -> if (-5 < -10) then 2 + x + y else 1) : Int -> (Int -> Int)"
test5 = "(\\x -> if (-5 < -10) then 2 + x else 1) : Int -> Int"
test6 = "(\\f -> \\x -> f (f x)) : (Bool -> Bool) -> (Bool -> Bool)"

-- Your top-level parser to test
myParser =
  exprParser

-- DO NOT MODIFY ANYTHING BELOW THIS COMMENT

exprParser : Parser Expr
exprParser =
      Parser.oneOf
        [ 
              succeed If 
                |. symbol "if"
                |. spaces
                |= lazy (\ () -> exprParser)
                |. spaces
                |. symbol "then"
                |. spaces
                |= lazy (\ () -> exprParser)
                |. spaces
                |. symbol "else"
                |. spaces
                |= lazy (\ () -> exprParser)
            ,  backtrackable <| succeed FunDecl 
                |. spaces
                |. symbol "\\"
                |. spaces
                |= Parser.variable
                    { start = Char.isLower
                    , inner = \c -> Char.isAlpha c || c == '_'
                    , reserved = Set.fromList [ "if", "then", "else", "True", "False", "Int", "Bool"]
                    }
                |. spaces 
                |. symbol "->"
                |. spaces
                |= lazy (\ () -> exprParser)
                |. spaces
            , backtrackable <| succeed FunApp 
                |. spaces
                |= factor
                |. spaces
                |= factor
                |. spaces
            , term
            , problem "not a constant or variable"
        ]

term : Parser Expr
term = 
  oneOf
    [
       backtrackable <| succeed (\e1 op e2 -> BinOp op e1 e2) 
                |. spaces
                |= factor
                |. spaces
                |= opParser
                |. spaces
                |= lazy ( \ () -> exprParser )
                |. spaces
    , backtrackable <| succeed (\e1 op e2 -> PredOp op e1 e2)  
                |. spaces
                |= factor
                |. spaces
                |= predOpParser
                |. spaces
                |= factor
                |. spaces
    , factor
        |. spaces
        |> andThen (\fac -> oneOf
        [ succeed (Ann fac)
                |. symbol ":"
                |. spaces
                |= tyParser
                |. spaces
        , succeed fac
        ])
    ]
  
factor : Parser Expr
factor =
  oneOf
    [
      succeed (Boolean True) 
                |. keyword "True"
    , succeed (Boolean False) 
                |. keyword "False"
    , succeed Integer 
         |= oneOf
           [ succeed negate
               |. symbol "-"
               |= int
           , int
           ]
    , succeed Var 
                |= Parser.variable
                    { start = Char.isLower
                    , inner = \c -> Char.isAlphaNum c || c == '_'
                    , reserved = Set.fromList [ "if", "then", "else", "True", "False", "Int", "Bool"]
                    }
    , succeed identity
        |. symbol "("
        |. spaces
        |= lazy ( \ () -> exprParser )
        |. spaces
        |. symbol ")"
    ]

opParser : Parser Op
opParser =
  Parser.oneOf
    [ succeed Plus
        |. symbol "+"
    , succeed Minus
        |. symbol "-"
    , succeed Mult
        |. symbol "*"
    , succeed Divide
        |. symbol "//"
    , problem "not an operation"
    ]


predOpParser : Parser PredOp
predOpParser =
   oneOf
     [succeed LTE
         |. symbol "<="
     , succeed GTE
         |. symbol ">="
     , succeed LT
         |. symbol "<"
     , succeed GT
         |. symbol ">"
     , succeed Eq
         |. symbol "=="
     , succeed And
         |. symbol "&&"
     , succeed Or
         |. symbol "||"
     , problem "not a PredOp"
     ]

tyParser : Parser Ty
tyParser =
  tyTerm
  |. spaces
  |> andThen (\ty -> oneOf
     [ succeed (FunTy ty)
           |. symbol "->"
           |. spaces
           |= tyTerm
           |. spaces
     , succeed ty
     ]
     )
 
tyTerm : Parser Ty
tyTerm =
    oneOf
      [
        succeed IntegerTy
           |. symbol "Int"
      , succeed BooleanTy
           |. symbol "Bool"
      , succeed identity
        |. symbol "("
        |. spaces
        |= lazy (\() -> tyParser)
        |. spaces
        |. symbol ")"
      ]

-- Helper for HTML table cells
cellH txt = Html.th [style "border" "1px solid", style "text-align" "left"] [Html.text txt]
cell txt = Html.td [style "border" "1px solid", style "text-align" "left"] [Html.text txt]

-- Helper for parsing, type inference and render to String
parseAndInferStr = 
  let
    join mRes = case mRes of 
      (Just p) -> p
      _ -> Nothing
  in
    Parser.run myParser 
    >> Result.toMaybe 
    >> Maybe.map (inferType Dict.empty)
    >> join
    >> Debug.toString
  
-- The view function
view model = 
  let
    parseResult = Parser.run myParser model.text
    parseResult2 = Parser.run myParser model.text
    typeCheckResult =
      case parseResult of
        Ok expr -> inferType Dict.empty expr
        _ -> Nothing
  in
  Html.div [] 
    [
      Html.h4 [] [Html.text "Input"]
    , Html.text "Type here to test different inputs"
    , Html.div[] [Html.textarea [A.value model.text, E.onInput NewString, A.rows 6] []]
    , Html.h4 [] [Html.text "Parser Output"]
    , Html.div [] [Html.text <| Debug.toString <| Parser.run myParser model.text]    
    , Html.h4 [] [Html.text "Type Checking Result"]
    , Html.div [] [Html.text <| Debug.toString <| typeCheckResult]
    , Html.h4 [] [Html.text "Some unit tests"]
    , Html.div []
      [ Html.table [style "border-spacing" "0", style "border-collapse" "collapse"]
          [ Html.tr [] [cellH "Test case", cellH "Expected", cellH "Actual"]
          , Html.tr [] [cell test0, cell "IntegerTy", cell (parseAndInferStr test0)]
          , Html.tr [] [cell test1, cell "IntegerTy", cell (parseAndInferStr test1)]
          , Html.tr [] [cell test2, cell "Just (FunTy IntegerTy IntegerTy)", cell (parseAndInferStr test2)]
          , Html.tr [] [cell test3, cell "Just BooleanTy", cell (parseAndInferStr test3)]
          , Html.tr [] [cell test4, cell "Just (FunTy IntegerTy (FunTy IntegerTy IntegerTy))", cell (parseAndInferStr test4)]
          , Html.tr [] [cell test5, cell "Just (FunTy IntegerTy IntegerTy)", cell (parseAndInferStr test5)]
          , Html.tr [] [cell test6, cell "Just (FunTy (FunTy BooleanTy BooleanTy) (FunTy BooleanTy BooleanTy))", cell (parseAndInferStr test6)]
          ]
      ]
    ]

-- Main
type Msg = NewString String

type alias Model = { text: String }

update msg model = case msg of
                    NewString newText -> { model | text = newText }

init = { text = myTestText }

main = Browser.sandbox { init = init, update = update, view = view }
