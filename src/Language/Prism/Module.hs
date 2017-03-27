{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}

module Language.Prism.Module
( -- * Declarations
  Declaration
, DeclarationF(..)
, Update(..)
, Scope(..)
  -- * Expressions
, Expression
, ExpressionF(..)
, BinaryOperator(..)
, Value(..)
, Type(..)
, typeOf
, Start(..)
, End(..)
, Name(..)
  -- ** Sugar
, constant
, var
, call
, (~>)
, (!~>)
, (.=!)
, (.=)
, (#)
, (!+!)
, (!-!)
, (!*!)
, (!/!)
, (!?!)
, (!==!)
, (!/=!)
, (!<=!)
, (!&&!)
, (!||!)
, (!<!)
, (!>!)
, (!>=!)
, intExp
, certainly
  -- ** Value shorthands
, double
, int
  -- ** Relational shorthands
, equals
, notEquals
, lessThanEquals
, not
, and
, or
, lessThan
, greaterThan
, greaterThanEquals
  -- * Reexports
, cata
, Text
, Fix(..)
) where

import Fix

import Data.String
import Data.Text ( Text )

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
    Type
    Value
  | Action
    (Maybe Name) -- ^ name for the action
    Expression -- ^ guard
    [(Expression, Update)] -- ^ the probability and the updates to do
  | Module
    Name
    [next]
  deriving (Eq, Functor, Ord, Read, Show)

action :: Expression -> [(Expression, Update)] -> Maybe Name -> Declaration
action guard update n = Fix (Action n guard update)

(~>) :: Expression -> [(Expression, Update)] -> Name -> Declaration
(~>) e u n = action e u (Just n)
infixl 7 ~>

(!~>) :: Expression -> [(Expression, Update)] -> Declaration
(!~>) e u = action e u Nothing
infixl 7 !~>

constantDecl :: Name -> Value -> Declaration
constantDecl n v = Fix (ConstantDecl n v)

(.=!) :: Name -> Value -> Declaration
(.=!) = constantDecl
infixl 7 .=!

variableDecl :: Name -> (Type, Value) -> Scope -> Declaration
variableDecl n (t, v) s = Fix (VariableDecl s n t v)

(.=) :: Name -> (Type, Value) -> Scope -> Declaration
(.=) = variableDecl
infixl 7 .=

(#) :: a -> (a -> b) -> b
(#) = flip ($)
infixl 6 #

intExp :: Int -> Expression
intExp i = Fix (Constant (int i))

certainly :: Update -> [(Expression, Update)]
certainly u = [(intExp 1, u)]

data Scope
  = Global
  | Local
  deriving (Eq, Ord, Read, Show)

data Update
  = Update [(Name, Expression)]
  | Noop
  deriving (Eq, Ord, Read, Show)

type Declaration = Fix DeclarationF

equals :: Expression -> Expression -> Expression
equals e1 e2 = Fix (BinaryOperator Equals e1 e2)

(!==!) :: Expression -> Expression -> Expression
(!==!) = equals
infix 4 !==!

notEquals :: Expression -> Expression -> Expression
notEquals e1 e2 = Fix (BinaryOperator NotEquals e1 e2)

(!/=!) :: Expression -> Expression -> Expression
(!/=!) = notEquals
infix 4 !/=!

lessThanEquals :: Expression -> Expression -> Expression
lessThanEquals e1 e2 = Fix (BinaryOperator LessThanEquals e1 e2)

(!<=!) :: Expression -> Expression -> Expression
(!<=!) = lessThanEquals
infix 4 !<=!

not :: Expression -> Expression
not = Fix . Not

and :: Expression -> Expression -> Expression
and g1 g2 = Fix (BinaryOperator And g1 g2)

(!&&!) :: Expression -> Expression -> Expression
(!&&!) = and
infixr 3 !&&!

or :: Expression -> Expression -> Expression
or g1 g2 = Fix (BinaryOperator Or g1 g2)

(!||!) :: Expression -> Expression -> Expression
(!||!) = or
infixr 2 !||!

lessThan :: Expression -> Expression -> Expression
lessThan e1 e2 = Fix (BinaryOperator LessThan e1 e2)

(!<!) :: Expression -> Expression -> Expression
(!<!) = lessThan
infix 4 !<!

greaterThan :: Expression -> Expression -> Expression
greaterThan e1 e2 = Fix (BinaryOperator GreaterThan e1 e2)

(!>!) :: Expression -> Expression -> Expression
(!>!) = greaterThan
infix 4 !>!

greaterThanEquals :: Expression -> Expression -> Expression
greaterThanEquals e1 e2 = Fix (BinaryOperator GreaterThan e1 e2)

(!>=!) :: Expression -> Expression -> Expression
(!>=!) = greaterThanEquals
infix 4 !>=!

data ExpressionF next
  = Constant Value
  | Variable Name
  | BinaryOperator BinaryOperator next next
  | Call Name [next]
  | Not next
  | Ternary next next next
  deriving (Eq, Functor, Ord, Read, Show)

constant :: Value -> Expression
constant = Fix . Constant

var :: Name -> Expression
var = Fix . Variable

call :: Name -> [Expression] -> Expression
call n args = Fix (Call n args)

instance IsString (Fix ExpressionF) where
  fromString = Fix . Variable . fromString

type Expression = Fix ExpressionF

op :: BinaryOperator -> Expression -> Expression -> Expression
op o e1 e2 = Fix (BinaryOperator o e1 e2)

(!+!) :: Expression -> Expression -> Expression
(!+!) = op Add
infixl 6 !+!

(!-!) :: Expression -> Expression -> Expression
(!-!) = op Subtract
infixl 6 !-!

(!*!) :: Expression -> Expression -> Expression
(!*!) = op Multiply
infixl 7 !*!

(!/!) :: Expression -> Expression -> Expression
(!/!) = op Divide
infixl 7 !/!

(!?!) :: Expression -> Expression -> Expression -> Expression
(!?!) e1 e2 e3 = Fix (Ternary e1 e2 e3)
infixr 1 !?!

data BinaryOperator
  = Add
  | Subtract
  | Multiply
  | Divide
  | And
  | Or
  | LessThanEquals
  | LessThan
  | GreaterThanEquals
  | GreaterThan
  | Equals
  | NotEquals
  deriving (Eq, Ord, Read, Show)

data Value
  = IntegerValue Int
  | DoubleValue Double
  | BooleanValue Bool
  deriving (Eq, Ord, Read, Show)

data Type
  = IntegerType
  | DoubleType
  | EnumType Start End
  | BooleanType
  deriving (Eq, Ord, Read, Show)

typeOf :: Value -> Type
typeOf = \case
  IntegerValue _ -> IntegerType
  DoubleValue _ -> DoubleType
  BooleanValue _ -> BooleanType

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

instance IsString Name where
  fromString = Name . fromString
