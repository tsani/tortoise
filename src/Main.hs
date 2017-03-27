module Main where

import qualified Data.Text.IO as T
import Language.Prism.Pretty
import Language.Prism.Codegen

initSet :: InitSettings
initSet = InitSettings
     { numBots = 10000
     , baseLevels = [(1, 10), (2, 60), (3, 37)]
     , efficiency = 1.0
     , exponentArg = 1.0
     }

main :: IO ()
main = T.putStrLn $ pretty $ codegen initSet
