module Main where

import qualified Data.Text.IO as T
import Language.Prism.Module
import Language.Prism.Pretty
import Language.Prism.Codegen

initSet :: InitSettings
initSet = InitSettings
     { numBots = 100
     , baseLevels = [(1, 10), (2, 60), (3, 37)]
     }

main :: IO ()
main = T.putStr $ pretty $ codegen initSet
