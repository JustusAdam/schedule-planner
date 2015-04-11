module Main where

import           Calculator
import qualified Data.Map   as Map
import Scale


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
  sequence $ map print lessons
  putStrLn "\n"
  let calc = Calculator.calc lessons

  pc calc
  return ()
  where
    pc = \y -> sequence $ map (\x -> putStrLn ("\n\n" ++ (formatSchedule x))) y
