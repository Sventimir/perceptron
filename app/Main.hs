module Main where

import System.IO (readFile)
import Data.String (lines)

import Sample
import Perceptron

epoch :: Int
epoch = 20

trainingData :: IO [Sample Double]
trainingData =
    fmap (fmap readSample . lines) $ readFile "./data/trainingData.txt"


trySample :: (Perceptron p, Show (p Double)) => (p Double, Int, Double) -> (Double, Double, Double) -> (p Double, Int, Double)
trySample (p, hitCount, squaredError) (t, s, d) =
    let (p', err) = train p [t, s] d in
    (p', if abs err < 0.5 then succ hitCount else hitCount, squaredError + (err ** 2 / 2))

iterateTraining :: (Perceptron p, Show (p Double)) => [(Double, Double, Double)] -> Int -> p Double -> IO (p Double)
iterateTraining _ 0 p = return p
iterateTraining samples i p = do
        let (trainedP, hitCount, squaredError) = foldl trySample (p, 0, 0) samples
        let sampleCount = fromIntegral $ length samples
        let percentError = (sampleCount - fromIntegral hitCount) / sampleCount
        putStrLn $
            "TOTAL ERROR AFTER " ++ show (epoch - i) ++
            "  percent error:" ++ show percentError ++
            "  square error:" ++ show squaredError ++ "."
        print trainedP
        iterateTraining samples (if i > 0 then pred i else succ i) trainedP


boolInput :: Bool -> Double
boolInput True = 1
boolInput False = 0

main :: IO ()
main = do
    samples <- trainingData

    let tempSet = normalize $ fmap temp samples
    let seasonSet = normalize $ fmap (seasonNum True . season) samples
    let normalized = zip3 tempSet seasonSet (fmap (boolInput . decision) samples)

    putStrLn "Training data:"
    sequence_ $ fmap print normalized
    p <- iterateTraining normalized epoch $ biasedSigmoid [(-1), 1]
    return ()
