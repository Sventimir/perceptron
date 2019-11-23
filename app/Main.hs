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
    putStrLn "Welcome to Perceptron!"
    trainingData >>= (sequence_ . fmap print)
