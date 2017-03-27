{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Prism.Codegen where

import Fix
import Prelude hiding ( and )

import Data.Monoid ( (<>) )
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Language.Prism.Module

-- | Initial settings for the entire engagement.
data InitSettings
  = InitSettings
  { numBots :: Int
  , baseLevels :: [(Int, Int)]
  -- ^ Pairs of monster IDs and their corresponding
  -- base levels.
  , exponentArg :: Double
  -- ^ Exponent used in g(x) = x^a
  , efficiency :: Double
  -- ^ Efficiency of robots
  }

declSettings :: InitSettings -> [Declaration]
declSettings InitSettings{..}
  = Fix (ConstantDecl "N" (int numBots)) :
  (Fix (ConstantDecl "e" (double 2.71828))) :
  (Fix (ConstantDecl "a" (double exponentArg))) :
  (Fix (ConstantDecl "c" (double efficiency))) : 
  declLevels numBots baseLevels

declLevels :: Int -> [(Int, Int)] -> [Declaration]
declLevels _ [] = []
declLevels m ((l, r):ls)
  = Fix 
    (VariableDecl Global (enemyLevelName l) (EnumType (Start 0) (End r)) (int r))
  -- ^ Constant enemy base level
  -- : Fix (VariableDecl Global
  --   (enemyAdjustedLevelName l) (int r))
  -- ^ Global var for enemy adjusted level. Starts
  -- at the same level because zero.
  : Fix (VariableDecl Global
    (numBotsName l) (EnumType (Start 0) (End m)) (int 0))
  -- ^ Global var for bots currently on this enemy.
  : declLevels m ls

enemyLevelName :: Int -> Name
enemyLevelName i = Name ("enemy_base_level_" <> T.pack (show i))

enemyAdjustedLevelName :: Int -> Name
enemyAdjustedLevelName i = Name ("enemy_adj_level_" <> T.pack (show i))

numBotsName :: Int -> Name
numBotsName i = Name ("num_bots_" <> T.pack (show i))

moduleName :: Int -> Name
moduleName i = Name ("battle_" <> T.pack (show i))

-- |
moduleTemplate
  :: Int -- ^ The current enemy ID.
  -> [Int] -- ^ The entire list of enemy IDs.
  -> Declaration -- ^ The resulting module definition for this battle.
moduleTemplate i es
  = Fix $ Module (moduleName i) $ moduleDecls i es

state :: Int -> Expression
state i = Fix (Variable $ Name $ L.toStrict $ "s_" `L.append` (L.pack $ show i))

intExp :: Int -> Expression
intExp i = Fix (Constant (int i))

-- | Produces the appropriate update from the given
-- variable containing the bot count from which we
-- need to compute a proportion to 'receive' in this
-- battle.
--
-- This function does not produce the update to
-- correspondingly reduce the number of originating bots.
updateBotCount
  :: Name
  -- ^ Variable containing the total number of bots
  -- that will be distributed.
  -> Int -- ^ Current enemy ID. This receives a proportion of the bots.
  -> [Int] -- ^ All enemy IDs
  -> (Int -> Expression)
  -- ^ Function mapping enemy IDs
  -> (Name, Expression)
  -- ^ Update that uses the appropriate update function to produce the "weight" of a particular enemy.
  -- an update
updateBotCount origin i es g
  = (numBotsName i, updateExpression origin i es g)

-- | The update expression in the RHS of a command.
updateExpression
  :: Name -- ^ Origin of bots
  -> Int -- ^ ID of enemy receiving bots
  -> [Int] -- ^ The IDs in consideration.
  -> (Int -> Expression) -- ^ g(x), used in the summation.
  -> Expression
  -- ^ Expression that evaluates to number of bots received
  -- by enemy.
updateExpression n i es g
  = Fix $ BinaryOperator Add (Fix $ Variable $ numBotsName i) $
    Fix $ Call "floor" 
    [ Fix $ BinaryOperator Multiply (Fix $ Variable n) $ Fix $ BinaryOperator Divide
      (g i)
      (summation $ fmap (\x -> g x) es)
    ]

-- | Example of a g(x) scaling function (scales adjusted level)
-- for a given enemy ID.
scale
  :: Int
  -- ^ ID of enemy
  -> Expression
  -- ^ g(x), the scaled adjusted level
scale i
  = Fix $ Call "pow" [adjLevelExp i, Fix $ Variable "a"]

-- | Expression representing adjusted level of enemy i
adjLevelExp :: Int -> Expression
adjLevelExp i = Fix $
  BinaryOperator Divide (Fix $ Variable (enemyLevelName i))
  (Fix $ BinaryOperator Add
    (Fix $ BinaryOperator Multiply (Fix $ Variable "c") (Fix $ Variable (numBotsName i)))
    (Fix $ Constant $ int 1)
  )

-- | Provides the summation over a list of expressions.
summation :: [Expression] -> Expression
summation [] = error "Empty list passed to summation."
summation [x] = x
summation (x:xs) = Fix $ BinaryOperator Add x (summation xs)

-- | Provides the updates for the initial step.
initDistribute
  :: Int    -- ^ Current enemy ID
  -> [Int]  -- ^ List of enemy IDs
  -> [(Expression, Update)]
  -- ^ Initial distribution of bots from N that go to
  -- this battle.
initDistribute i es
  = [(intExp 1, Update [ updateBotCount "N" i es idBaseLevel
                       , (Name $ L.toStrict $ "s_" <> L.pack (show i), intExp 1)
                       ])]

-- | Provides simply the base level of the
-- given enemy ID.
idBaseLevel :: Int -> Expression
idBaseLevel = Fix . Variable . enemyLevelName

-- | Provide updates for attacking action in attacking state.
attackUpdates
  :: Int
  -- ^ The current enemy ID.
  -> [(Expression, Update)]
  -- ^ The updates that can occur when attacking in attacking state.
attackUpdates i
  = let prob = expCdf $ adjLevelExp i
    in [ (prob, Noop)
       , ( Fix $ BinaryOperator Subtract (intExp 1) prob
         , Update [(Name $ L.toStrict $ "s_" <> L.pack (show i), intExp 2)]
         )
       ]

-- | Take adjusted level and return expression of f(x) = 1 - exp(-x)
-- evaluated at adjustedLevel.
expCdf :: Expression -> Expression
expCdf adjustedLevel = Fix $ BinaryOperator Subtract (intExp 1)
  $ Fix $ Call "pow" [Fix $ Variable "e", Fix $ BinaryOperator Subtract (intExp 0) adjustedLevel]

-- | Provides the redistribution policy given enemy ID
-- and rest of IDs. Updates must be performed on each enemy except
-- for the current one.
redisPolicy
  :: Int
  -> [Int]
  -> [(Expression, Update)]
redisPolicy i es = [(intExp 1, Update $ move' : lvl : u)] where
  es' = filter (/= i) es
  u = map (\x -> updateBotCount (numBotsName i) x es' scale) es'
  lvl = resetLevel i
  move' = (Name $ L.toStrict $ "s_" <> L.pack (show i), intExp 3)
  -- ^ Move to state 3

resetLevel :: Int -> (Name, Expression)
resetLevel i = (enemyLevelName i, intExp 0)

-- | Produce the contents of
-- the module for the corresponding enemy.
moduleDecls
  :: Int   -- ^ The enemy ID
  -> [Int] -- ^ All enemy IDs
  -> [Declaration] -- ^ The module contents
moduleDecls i es =
  (Fix (VariableDecl Local (Name $ L.toStrict $ "s_" <> L.pack (show i)) (EnumType (Start 0) (End 4)) (int 0))) :
  (Fix (Action Nothing (state i `equals` (intExp 0)) (initDistribute i es))) :
  (Fix (Action (Just "attack") (state i `equals` (intExp 1)) (attackUpdates i))) :
  (Fix (Action Nothing ((state i `equals` (intExp 2)) `and` (otherLevels i es `notEquals` intExp 0)) (redisPolicy i es))) :
  -- ^ Case where summation over other states is non-zero
  (Fix (Action Nothing ((state i `equals` (intExp 2)) `and` (otherLevels i es `equals` intExp 0)) [(intExp 1, Update $ [move i 3])])) :
  -- ^ Case where summation over other states is zero
  (Fix (Action (Just "attack") (state i `equals` (intExp 3)) [(intExp 1, Noop)])) :
  (Fix (Action (Just "done") (state i `equals` (intExp 3)) [(intExp 1, Update [(Name $ L.toStrict $ "s_" <> L.pack (show i), intExp 4)])])) : []

-- | Move state variable to j.
move :: Int -> Int -> (Name, Expression)
move i j = (Name $ L.toStrict $ "s_" <> L.pack (show i), intExp j)

-- | Return expression that computes the sum
-- of levels of all enemies that aren't the given
-- 'i'.
otherLevels
  :: Int
  -> [Int]
  -> Expression
otherLevels i es
  = let es' = filter (/= i) es
    in summation $ fmap (Fix . Variable . enemyLevelName) es'

-- | Generate list of declarations containing all globals,
-- module definitions, and the resulting synchronized TS.
codegen :: InitSettings -> [Declaration]
codegen set
  = declSettings set ++ (genModules $ fst <$> (baseLevels set))

-- | Produce list of declarations corresponding to the
-- composed synchronized transition system.
-- synchronize
--   :: [Int]
--   -- ^ List of all enemy IDs.
--   -> [Declaration]
--   -- ^ The synchronized declaration.
-- synchronize xs = intercalate " || " map (unName . moduleName) xs

-- | List of enemy IDs to a list of module
-- declarations (and renamings) followed by
-- the concurrent product of all modules.
genModules :: [Int] -> [Declaration]
genModules [] = []
genModules xs = map (\x -> Fix $ Module (moduleName x) (moduleDecls x xs)) xs
