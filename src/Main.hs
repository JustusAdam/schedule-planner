module Main where

import Calculator

main :: IO()
main = do
  let lessons = [(Lesson 1 1 2 "TGI"),(Lesson 1 1 2 "FS"),(Lesson 1 1 1 "TGI"),(Lesson 2 2 3 "TGI"),(Lesson 1 1 3 "RA"),(Lesson 2 2 2 "RA"),(Lesson 1 2 3 "FS")]
  putStrLn "\n"
  sequence (map print lessons)
  putStrLn "\n"
  let calc = map show (Calculator.calc lessons)
  sequence (map putStrLn calc)
  return ()
