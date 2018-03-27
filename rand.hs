import System.Random
newRand = randomIO :: IO Int

randomList :: Int -> [Int]
randomList seed = randomRs (1, 6) (mkStdGen seed) :: [Int]



main :: IO ()
main = do
    -- num :: Float
    num <- randomIO :: IO Int
    print . take 4 $ randomList num

