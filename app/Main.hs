module Main where

import Monopoly
import System.Random

randomList :: GameState Int -> [Int]
randomList seed = randomRs (1, 6) (mkStdGen seed) :: [Int]

main :: IO ()
main = do
    num <- randomIO :: IO Int 
    print . take 4 $ randomList num 
    images <- loadImages
    startGame images
