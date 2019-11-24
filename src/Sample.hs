module Sample (
    Season(..),
    Sample(..),
    readSample,
    seasonNum
) where

import Data.String

data Season = Spring | Summer | Autumn | Winter
    deriving (Show, Read, Enum)

data Sample a = Sample {
    temp :: a,
    season :: Season,
    decision :: Bool
} deriving Show

readSample :: Read a => String -> Sample a
readSample s = case words s of
    [tempStr, seasonStr, decStr] -> Sample {
            temp = read tempStr,
            season = read seasonStr,
            decision = intToBool $ read decStr
        }
    _ -> error $ "Bad sample '" ++ s ++ "'!"

    where
    intToBool 0 = False
    intToBool _ = True


-- Seasons can be numbered in a number of ways. Author of the original
-- Perceptron included numberings [1..4] and [4,3..1], which seems a little bit
-- strange, as I find numbering starting from 0 more natural. Anyways, original
-- numberings can be obtained from this function as: (seasonNum False 1)
-- and (seasonNum True 1) respectively.
seasonNum :: Num a => Bool -> Int -> Season -> a
seasonNum invert from season =
    let seasonNum = if invert
        then 3 - fromEnum season
        else fromEnum season
    in
    fromIntegral . (+) from . $ mod seasonNum 4
