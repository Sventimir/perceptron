module Sample (
    Season(..),
    Sample(..),
    readSample
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
