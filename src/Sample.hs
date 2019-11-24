module Sample (
    Season(..),
    Sample(..),
    readSample,
    seasonNum,
    normalize
) where

import Data.String

data Season = Summer | Spring | Autumn | Winter
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

-- Author of the original program counts seasons from 1 to 4 instead of a more
-- natural numbering 0-3. However, this does not really matter due to later
-- normalization. Therefore I preserve original numbering.
seasonNum :: Num a => Bool -> Season -> a
seasonNum invert season =
    let seasonNum = if invert
        then 3 - fromEnum season
        else fromEnum season
    in
    fromIntegral . succ $ mod seasonNum 4

-- Given minimum and maximum value in the sample, turn it proportionally into a
-- number between 0 and 1.
normalizeSample :: (Ord a, Fractional a) => a -> a -> a -> a
normalizeSample minVal maxVal val = (val - minVal) / (maxVal - minVal)

normalize :: (Ord a, Fractional a) => [a] -> [a]
normalize samples =
        fmap (normalizeSample (minimum samples) (maximum samples)) samples
