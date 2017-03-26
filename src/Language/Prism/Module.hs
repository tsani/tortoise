{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}

module Language.Prism.Module where

import Fix

import Data.Text ( Text )
import qualified Data.Text as T

import Prelude hiding ( and, or, not )

data DeclarationF next
  -- | e.g. @const double init_coal = 10@
  = ConstantDecl
    Name
    Value
  -- | e.g. @s : [0..7] init 0@
  | VariableDecl
    Scope
    Name
    Value
  | Action
    Name
    Guard
    [(Probability, Update)]
  | Module
    Name
    [next]
  deriving (Eq, Functor, Ord, Read, Show)

newtype Probability = Probability Double
  deriving (Eq, Ord, Read, Show)

data Update
  = Update
    { updateVar :: Name
    , updateExpr :: Expression
    }
  deriving (Eq, Ord, Read, Show)

type Declaration = Fix DeclarationF

data GuardF next
  = Equals Expression Expression
  | LessOrEquals Expression Expression
  | Not next
  | And next next
  | Or next next
  deriving (Eq, Functor, Ord, Read, Show)

type Guard = Fix GuardF

equals :: Expression -> Expression -> Guard
equals e1 e2 = Fix (Equals e1 e2)

notEquals :: Expression -> Expression -> Guard
notEquals e1 e2 = not (equals e1 e2)

lessThanOrEquals :: Expression -> Expression -> Guard
lessThanOrEquals e1 e2 = Fix (LessOrEquals e1 e2)

not :: Guard -> Guard
not = Fix . Not

and :: Guard -> Guard -> Guard
and g1 g2 = Fix (And g1 g2)

or :: Guard -> Guard -> Guard
or g1 g2 = Fix (Or g1 g2)

lessThan :: Expression -> Expression -> Guard
lessThan e1 e2 = lessThanOrEquals e1 e2 `and` not (equals e1 e2)

greaterThan :: Expression -> Expression -> Guard
greaterThan e1 e2 = not (lessThanOrEquals e1 e2)

greaterThanOrEquals :: Expression -> Expression -> Guard
greaterThanOrEquals e1 e2 = not (lessThan e1 e2)

data ExpressionF next
  = Constant Value
  | Variable Name
  | BinaryOperator BinaryOperator next next
  deriving (Eq, Ord, Read, Show)

type Expression = Fix ExpressionF

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  deriving (Eq, Ord, Read, Show)

data Scope
  = Global
  | Local
  deriving (Eq, Ord, Read, Show)

data Value
  = IntegerValue Int
  | DoubleValue Double
  | EnumValue Start End
  | Boolean Bool
  deriving (Eq, Ord, Read, Show)

newtype Start = Start Int
  deriving (Eq, Ord, Read, Show)

newtype End = End Int
  deriving (Eq, Ord, Read, Show)

double :: Double -> Value
double = DoubleValue

int :: Int -> Value
int = IntegerValue

newtype Name = Name { unName :: Text }
  deriving (Eq, Ord, Read, Show)
