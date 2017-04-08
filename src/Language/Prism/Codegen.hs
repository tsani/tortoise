{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Prism.Codegen where

import Fix
import Prelude hiding ( and , not , or )

import Data.Monoid ( (<>) )
import Data.String ( fromString )
import qualified Data.Text as T
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
  , lethality :: Double
  -- ^ Lethality of a round of combat. Must be in [0,1].
  }

-- | Produces the PRISM file preamble constants.
declSettings :: InitSettings -> [Declaration]
declSettings InitSettings{..}
  = Global # "N" .= (EnumType (Start 0) (End numBots), (int numBots))
  : "e" .=! double 2.71828
  : "a" .=! double exponentArg
  : "c" .=! double efficiency
  : "initialN" .=! int numBots
  : "b" .=! (double lethality)
  : "some_battle_won" #= disjunction (map (wonAgainst . fst) baseLevels)
  : "some_crit" #= disjunction (map (isCrit . fst) baseLevels)
  : declLevels numBots baseLevels
  where
    wonAgainst i = var (state i) !==! intExp 3
    isCrit i
      = always (
        (s !==! intExp 1) `implies` next (s !==! intExp 2)
      ) where
        s = var (state i)

-- | Produces the PRISM file globals for keeping track
-- of changing values.
declLevels :: Int -> [(Int, Int)] -> [Declaration]
declLevels _ [] = []
declLevels m ((l, r):ls)
  = enemyLevelName l .=! (int r)
  -- ^ Constant enemy base level
  : Global # numBotsName l .= (EnumType (Start 0) (End m), int 0)
  -- ^ Global var for bots currently on this enemy.
  : Global # enemyDeadName l .= (BooleanType, bool False)
  -- ^ Global var for whether or not this enemy is dead.
  : declLevels m ls

-- | Gets the name for the variable representing the level of a particular
-- enemy.
enemyLevelName :: Int -> Name
enemyLevelName i = Name ("enemy_base_level_" <> T.pack (show i))

enemyDeadName :: Int -> Name
enemyDeadName i = Name ("enemy_dead_" <> T.pack (show i))

-- | Gets the name for the variable representing the number of bots attacking a
-- particular enemy.
numBotsName :: Int -> Name
numBotsName i = Name ("num_bots_" <> T.pack (show i))

-- | Gets the name for the module representing a particular battle.
moduleName :: Int -> Name
moduleName i = Name ("battle_" <> T.pack (show i))

moduleTemplate
  :: Int -- ^ The current enemy ID.
  -> [Int] -- ^ The entire list of enemy IDs.
  -> Declaration -- ^ The resulting module definition for this battle.
moduleTemplate i es
  = Fix $ Module (moduleName i) $ moduleDecls i es

state :: Int -> Name
state i = fromString $ "s_" <> show i

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
-- Does not add the previous amount.
updateExpression
  :: Name -- ^ Origin of bots
  -> Int -- ^ ID of enemy receiving bots
  -> [Int] -- ^ The IDs in consideration.
  -> (Int -> Expression) -- ^ g(x), used in the summation.
  -> Expression
  -- ^ Expression that evaluates to number of bots received
  -- by enemy.
updateExpression n i es g
  = call "floor" [ var n !*! (g i !/! summation (g <$> es)) ]

-- | Example of a g(x) scaling function (scales adjusted level)
-- for a given enemy ID.
scale
  :: Int
  -- ^ ID of enemy
  -> Expression
  -- ^ g(x), the scaled adjusted level
scale i
  = var (enemyDeadName i) !?! intExp 0 $ call "pow" [adjLevelExp i, "a"]

-- | Expression representing adjusted level of enemy i
adjLevelExp :: Int -> Expression
adjLevelExp i
  = var (enemyLevelName i) !/! ("c" !*! var (numBotsName i) !+! intExp 1)

-- | Provides the summation over a list of expressions.
summation :: [Expression] -> Expression
summation [] = error "Empty list passed to summation."
summation [x] = x
summation (x:xs) = x !+! summation xs

-- | Provides the updates for the initial step.
initDistribute
  :: Int    -- ^ Current enemy ID
  -> [Int]  -- ^ List of enemy IDs
  -> [(Expression, Update)]
  -- ^ Initial distribution of bots from N that go to
  -- this battle.
initDistribute i es =
  [ ( intExp 1
    , Update
      [ updateBotCount "N" i es idBaseLevel
      , (state i, intExp 1)
      ]
    )
  ]

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
    in [ (prob, Update [move i 5])
       , ( intExp 1 !-! prob
         , Update [(state i, intExp 2)]
         )
       ]

-- | Take adjusted level and return expression of f(x) = 1 - exp(-x)
-- evaluated at adjustedLevel.
expCdf :: Expression -> Expression
expCdf adjustedLevel
  = intExp 1 !-! call "pow" [ "e", intExp 0 !-! adjustedLevel ]

-- | Provides the redistribution policy given enemy ID
-- and rest of IDs. Updates must be performed on each enemy except
-- for the current one.
redisPolicy
  :: Int
  -> [Int]
  -> [(Expression, Update)]
redisPolicy i es = [(intExp 1, Update $ move' : kill : u)] where
  es' = filter (/= i) es
  u = map (\x -> updateBotCount "N" x es' scale) es'
  move' = (state i, intExp 3)
  -- ^ Move to state 3
  kill = setDead i

-- | Provide the update for setting the 'dead' flag on
-- the ith enemy.
setDead :: Int -> (Name, Expression)
setDead i = (enemyDeadName i, constant $ bool True)

-- | Produce the contents of
-- the module for the corresponding enemy.
moduleDecls
  :: Int   -- ^ The enemy ID
  -> [Int] -- ^ All enemy IDs
  -> [Declaration] -- ^ The module contents
moduleDecls i es =
  [ Local # state i .= (EnumType (Start 0) (End 5), int 0)
  , inState 0 !~> initDistribute i es
  , "attack" # ((inState 1) `and` (var (numBotsName i) !>! intExp 0)) ~> attackUpdates i
  , "attack" # ((inState 1) `and` (var (numBotsName i) !==! intExp 0)) ~> [(intExp 1, Noop)]
  , ((inState 2) !&&! (not $ allOtherDead i es)) !~> redisPolicy i es
  -- ^ Case where summation over other states is non-zero
  ,
    (inState 2 !&&! allOtherDead i es)
    !~>
    certainly (Update [move i 3])
  -- ^ Case where summation over other states is zero
  , "attack" # ((inState 3) !&&! (not $ allOtherDead i es)) ~> certainly Noop
  , "done" # inState 3 ~> certainly (Update [move i 4])
  , ((inState 5) `and` (var (numBotsName i) !>! intExp 0) `and` (var "N" !>! intExp 0)) !~> (suffer i)
  ]
  where
    inState n = var (state i) !==! intExp n
    suffer i' = [ ( var "b"
                  , Update [ move i' 1
                           , (numBotsName i', var (numBotsName i') !-! intExp 1)
                           , ("N", var "N" !-! intExp 1)
                           ]
                  )
                , ( intExp 1 !-! (var "b")
                  , Update [ move i' 1 ]
                  )
                ]


-- | Move state variable to j.
move :: Int -> Int -> (Name, Expression)
move i j = (state i, intExp j)

-- | Return conjunction other whether all other enemies
-- aside from given one are dead.
allOtherDead
  :: Int
  -> [Int]
  -> Expression
allOtherDead i es = case filter (/= i) es of
  [] -> true
  es'@(_:_) -> conjunction (map (var . enemyDeadName) es')

disjunction :: [Expression] -> Expression
disjunction [] = error "Empty list passed to disjunction."
disjunction [x] = x
disjunction (x:xs) = x `or` disjunction xs

conjunction :: [Expression] -> Expression
conjunction [] = error "Empty list passed to conjunction."
conjunction [x] = x
conjunction (x:xs) = x `and` disjunction xs

-- | Generate list of declarations containing all globals,
-- module definitions, and the resulting synchronized TS.
codegen :: InitSettings -> Program
codegen set = Program DTMC rewards decls where
  rewards = [Rewards (Just "attacks") [ "attack" # true +=> intExp 1 ]]
  decls = declSettings set ++ (genModules $ fst <$> (baseLevels set))

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
