{-|
Module      : $Header$
Description : Interface for schedule-planner
Copyright   : (c) Justus Adam, 2015
License     : LGPL-3
Maintainer  : development@justusadam.com
Stability   : experimental
Portability : POSIX

main function and outside communication for this software
-}
module Main where

import           Calculator.Solver
import           Calculator.Scale
import qualified Data.Map.Lazy   as Map


-- getFromFile :: String -> IO()


main :: IO()
main = do
  let lessons = [
          Lesson 1 1 2 "TGI",
          Lesson 1 1 2 "FS",
          Lesson 1 1 1 "TGI",
          Lesson 2 2 3 "TGI",
          Lesson 1 1 3 "RA",
          Lesson 2 2 2 "RA",
          Lesson 1 2 3 "FS"
        ]
  putStrLn "\n"
  mapM print lessons
  putStrLn "\n"
  let calculated = calc lessons

  pc calculated
  return ()

  where
    pc = mapM (\x -> putStrLn ("\n\n" ++ formatSchedule x))
