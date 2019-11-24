module Main where

import System.IO (readFile)
import Data.String (lines)

import Sample
import Perceptron


trainingData :: IO [Sample Double]
trainingData =
    fmap (fmap readSample . lines) $ readFile "./data/trainingData.txt"

main :: IO ()
main = do
    putStrLn "Training data:"
    samples <- trainingData
    sequence_ $ fmap print samples

    putStrLn "Normalized temperatures:"
    let temps = fmap temp samples
    sequence_ . fmap print $ zip temps (normalize temps)

    putStrLn "Normalized seasons:"
    let seasons = fmap season samples
    sequence_ . fmap print $ zip seasons (normalize $ fmap (seasonNum False) seasons)
