{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Prism.Codegen where

import Fix

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
  }

declSettings :: InitSettings -> [Declaration]
declSettings InitSettings{..}
  = Fix (ConstantDecl "N" (int numBots))
  : declLevels baseLevels

declLevels :: [(Int, Int)] -> [Declaration]
declLevels [] = []
declLevels ((l, r):ls)
  = Fix 
    (VariableDecl Global (enemyLevelName l) IntegerType (int r))
  -- ^ Constant enemy base level
  -- : Fix (VariableDecl Global
  --   (enemyAdjustedLevelName l) (int r))
  -- ^ Global var for enemy adjusted level. Starts
  -- at the same level because zero.
  : Fix (VariableDecl Global
    (numBotsName l) IntegerType (int 0))
  -- ^ Global var for bots currently on this enemy.
  : declLevels ls

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
  -> (Name, Expression)
  -- ^ Update that uses the appropriate update function to produce
  -- an update
updateBotCount origin i es
  = (numBotsName i, updateExpression origin i es scale)

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
  = Fix $ BinaryOperator Multiply (Fix $ Variable n) $ Fix $ BinaryOperator Divide
    (g i)
    (summation $ fmap (\x -> g x) es)

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
  (Fix $ BinaryOperator Multiply (Fix $ Variable "c") (Fix $ Variable (numBotsName i)))

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
  = [(intExp 1, Update [updateBotCount "N" i es])]

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
  $ Fix $ Call "exp" [Fix $ BinaryOperator Subtract (intExp 0) adjustedLevel]

-- | Provides the redistribution policy given enemy ID
-- and rest of IDs. Updates must be performed on each enemy except
-- for the current one.
redisPolicy
  :: Int
  -> [Int]
  -> [(Expression, Update)]
redisPolicy i es = [(intExp 1, Update $ lvl : u)] where
  es' = filter (/= i) es
  u = map (\x -> updateBotCount (numBotsName i) x es') es'
  lvl = resetLevel i

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
  (Fix (Action Nothing (state i `equals` (intExp 2)) (redisPolicy i es))) :
  (Fix (Action (Just "attack") (state i `equals` (intExp 3)) [(intExp 1, Noop)])) :
  (Fix (Action (Just "done") (state i `equals` (intExp 3)) [(intExp 1, Update [(Name $ L.toStrict $ "s_" <> L.pack (show i), intExp 4)])])) : []

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
