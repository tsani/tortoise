{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Prism.Pretty where

import Language.Prism.Module

import Text.PrettyPrint.Leijen.Text
import qualified Text.PrettyPrint.Leijen.Text as PP

type P a = a -> Doc
type PF f = f Doc -> Doc

prettyDeclarationF
  :: P Update
  -> P Expression
  -> P Type
  -> P Value
  -> P Name
  -> PF DeclarationF
prettyDeclarationF ppUpdate ppExpr ppType ppVal ppName = \case
  Formula name expr -> "formula" <+> ppName name <+> "=" <+> ppExpr expr <> semi
  Label name expr -> "label" <+> dquotes (ppName name) <+> "=" <+> ppExpr expr <> semi
  ConstantDecl name val -> let name' = ppName name ; val' = ppVal val in
    case val of
      IntegerValue _ -> "const int" <+> name' <+> "=" <+> val' <> semi
      DoubleValue _ -> "const double" <+> name' <+> "=" <+> val' <> semi
      BooleanValue _ -> "const bool" <+> name' <+> "=" <+> val' <> semi
  VariableDecl scope name ty val ->
    let name' = ppName name ; val' = ppVal val ; ty' = ppType ty
    in case scope of
      Global -> "global" <+> name' <+> ":" <+> ty' <+> "init" <+> val' <> semi
      Local -> name' <+> ":" <+> ty' <+> "init" <+> val' <> semi
  Action name guard update -> let name' = maybe "" ppName name in
    "[" <> name' <> "]" <+> ppExpr guard <+> "->" <+>
      cat (
        punctuate " + " $
        map (\(e, u) -> ppExpr e <> ":" <> ppUpdate u) update
      )
    <> semi
  Module name decls ->
    "module" <+> ppName name <$$> indent 4 (vcat decls) <$$> "endmodule"

prettyUpdate
  :: P Expression
  -> P Name
  -> P Update
prettyUpdate ppExpr ppName = \case
  Update ps ->
    hcat . punctuate " & " .
    map (\(name, expr) -> parens (ppName name <> "'" <+> "=" <+> ppExpr expr)) $
    ps
  Noop -> "true"

prettyExpressionF
  :: P Value
  -> P Name
  -> P BinaryOperator
  -> P UnaryOperator
  -> PF ExpressionF
prettyExpressionF ppVal ppName ppBin ppUn = \case
  Constant v -> ppVal v
  Variable name -> ppName name
  BinaryOperator b e1 e2 -> parens (e1 <+> ppBin b <+> e2)
  UnaryOperator u e -> ppUn u <+> parens e
  Call name args -> parens (ppName name <> parens (hcat (punctuate ", " args)))
  Ternary e1 e2 e3 -> parens (e1 <+> "?" <+> e2 <+> ":" <+> e3)

prettyBinaryOperator :: P BinaryOperator
prettyBinaryOperator = \case
  Add -> "+"
  Subtract -> "-"
  Multiply -> "*"
  Divide -> "/"
  And -> "&"
  Or -> "|"
  LessThanEquals -> "<="
  LessThan -> "<"
  GreaterThanEquals -> ">="
  GreaterThan -> ">"
  Equals -> "="
  NotEquals -> "!="
  Implies -> "=>"

prettyUnaryOperator :: P UnaryOperator
prettyUnaryOperator = \case
  Not -> "!"
  Eventually -> "F"
  Always -> "G"
  Next -> "X"

prettyValue :: P Value
prettyValue = \case
  IntegerValue n -> PP.int n
  DoubleValue d -> PP.double d
  BooleanValue b -> if b then "true" else "false"

prettyName :: P Name
prettyName (Name name) = textStrict name

prettyType :: P Type
prettyType = \case
  IntegerType -> "int"
  DoubleType -> "double"
  EnumType (Start s) (End e) -> "[" <> PP.int s <> ".." <> PP.int e <> "]"
  BooleanType -> "bool"

prettyModel :: P Model
prettyModel DTMC = "dtmc"

prettyRewards :: P Name -> P Expression -> P Reward
prettyRewards ppName ppExpr (Reward n e1 e2) =
  maybe "" (brackets . ppName) n <+> ppExpr e1 <+> ":" <+> ppExpr e2 <> ";"

prettyRewardStructure :: P Name -> P Reward -> P RewardStructure
prettyRewardStructure ppName ppReward (Rewards n rs) =
  "rewards" <+> maybe "" (dquotes . ppName) n <$$>
  indent 4 (vcat (map ppReward rs)) <$$>
  "endrewards"

pretty :: Program -> Text
pretty = displayTStrict . renderPretty 1.0 maxBound . phi where
  phi (Program m rs ds)
    = prettyModel m
    <$$> vcat (map pRewardStructure rs)
    <$$> vcat (map pDeclaration ds)

  pReward = prettyRewards prettyName pExpression

  pRewardStructure = prettyRewardStructure prettyName pReward

  pExpression = cata f where
    f = prettyExpressionF
      prettyValue
      prettyName
      prettyBinaryOperator
      prettyUnaryOperator

  pUpdate = prettyUpdate pExpression prettyName

  pDeclaration
    = cata (
      prettyDeclarationF pUpdate pExpression prettyType prettyValue prettyName
    )
