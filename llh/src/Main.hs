module Main where

import Data.Char

main :: IO ()
main = do
  putStrLn "hello world"
  putStrLn (show (quadroot 1 2 3))
  putStrLn (show (multiplicationTable 10))
  putStrLn (capitalizeWords "i've seen things you wouldn't believe")
  putStrLn (capitalizeWords2 "attackships on fire off the shoulder of orion")
  putStrLn (capitalizeWords3 "time to die")


quadroot :: Double -> Double->Double->(Double,Double)
quadroot a b c = 
  let 
    d = sqrt b^2-4*a*c
  in
    (1/(2*a) * ((-1)* b) + d, 1/(2*a) * ((-1)* b) - d)

multiplicationTable :: Int-> [[Int]]
multiplicationTable limit = 
  let
    header = [1..limit]
    cols h x = map (x*) h
  in
    map (cols header) header

capitalizeWords :: [Char]->[Char]
capitalizeWords source = 
  let
    capitalizeWord w = toUpper (head w) : (tail w)
  in
    unwords (map capitalizeWord (words source))

capitalizeWords2 :: String -> String
capitalizeWords2 source = 
  let 
    capitalizeWord w =  toUpper (head w) : (tail w)
  in
    unwords [capitalizeWord w | w <- (words source)]


capitalizeWords3 :: String -> String
capitalizeWords3 source =
  let 
    capitalizeWord (w:ws) =  toUpper w : ws
    capitalizeWord [] = []
    foo x = do
      curWord <- words x
      return $ capitalizeWord curWord
  in unwords $ foo source
      
  
 

  