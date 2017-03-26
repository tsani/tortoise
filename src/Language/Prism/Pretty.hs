{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Prism.Pretty where

import Language.Prism.Module
import qualified Language.Prism.Module as P

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
  Action name guard updates -> let name' = maybe "" ppName name in
    "[" <> name' <> "]" <+> ppExpr guard <+> "->" <+>
      cat (
        punctuate " + " .
        map (\(e, us)
          -> ppExpr e <> ":" <> parens (
            hcat . punctuate " & " $ map ppUpdate us
          )
        ) $
        updates
      )
    <> semi
  Module name decls ->
    "module" <+> ppName name <$$> indent 4 (vcat decls) <$$> "endmodule"

prettyUpdate
  :: P Expression
  -> P Name
  -> P Update
prettyUpdate ppExpr ppName = \case
  Update name expr -> ppName name <> "'" <+> "=" <+> ppExpr expr
  Noop -> "true"

prettyExpressionF
  :: P Value
  -> P Name
  -> P BinaryOperator
  -> PF ExpressionF
prettyExpressionF ppVal ppName ppBin = \case
  Constant v -> ppVal v
  Variable name -> ppName name
  BinaryOperator b e1 e2 -> parens (e1 <+> ppBin b <+> e2)
  Call name args -> parens (ppName name <> parens (hcat (punctuate ", " args)))
  Not e -> parens ("!" <+> e)

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

pretty :: [Declaration] -> Doc
pretty = vcat . map phi where
  pExpression
    = cata (prettyExpressionF prettyValue prettyName prettyBinaryOperator)

  pUpdate = prettyUpdate pExpression prettyName

  phi
    = cata (
      prettyDeclarationF pUpdate pExpression prettyType prettyValue prettyName
    )
