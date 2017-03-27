module Main where

import qualified Data.Text.IO as T
import Language.Prism.Pretty
import Language.Prism.Codegen

import Options.Applicative

initBots :: Parser Int
initBots = option auto
  (  long "numbots"
  <> short 'n'
  <> metavar "NUM_BOTS"
  <> help "Number of initial bots"
  )

initEnemies :: Parser [(Int, Int)]
initEnemies = option auto
  ( long "enemies"
  <> short 'm'
  <> metavar "ENEMY_LIST"
  <> help "List of enemies in Haskell syntax"
  )

initEff :: Parser Double
initEff = option auto
  ( long "efficiency"
  <> short 'e'
  <> metavar "EFFICIENCY"
  <> help "Efficiency rating of robots"
  <> value 1.0
  )

initExpArg :: Parser Double
initExpArg = option auto
  ( long "exponent"
  <> short 'a'
  <> metavar "EXPONENT_ARGUMENT"
  <> help "Exponent argument in g(x) = x^a"
  <> value 1.0
  )

p :: Parser InitSettings
p = InitSettings 
  <$> initBots 
  <*> initEnemies
  <*> initEff
  <*> initExpArg

opts :: ParserInfo InitSettings
opts = info (p <**> helper)
  ( fullDesc
  <> progDesc "Generate PRISM code for swarm attack planning."
  <> header "tortoise - a PRISM swarm verification code generator"
  )

initSet :: InitSettings
initSet = InitSettings
     { numBots = 10000
     , baseLevels = [(1, 10), (2, 60), (3, 37)]
     , efficiency = 1.0
     , exponentArg = 1.0
     }

preamble :: String
preamble = "dtmc\nrewards\n    [attack] true : 1;\nendrewards\n"

main :: IO ()
main = do
  o <- execParser opts
  putStrLn preamble
  T.putStrLn $ pretty $ codegen o
