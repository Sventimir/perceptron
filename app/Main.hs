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


trySample :: Perceptron p => (p Double, Int, Double) -> (Double, Double, Double) -> (p Double, Int, Double)
trySample (p, hitCount, squaredError) (t, s, d) =
    let (p', err) = train p [t, s] d in
    (p', if err == 0 then succ hitCount else hitCount, squaredError + err)

iterateTraining :: Perceptron p => [(Double, Double, Double)] -> Int -> p Double -> IO (p Double)
iterateTraining _ 0 p = return p
iterateTraining samples i p = do
        let (trainedP, hitCount, squaredError) = foldl trySample (p, 0, 0) samples
        let sampleCount = fromIntegral $ length samples
        let percentError = (sampleCount - fromIntegral hitCount) / sampleCount
        putStrLn $
            "TOTAL ERROR AFTER " ++ show (epoch - succ i) ++
            "  percent error:" ++ show percentError ++
            "  square error:" ++ show squaredError ++ "."
        iterateTraining samples (if i > 0 then pred i else succ i) trainedP


boolInput :: Bool -> Double
boolInput True = 1
boolInput False = 0

main :: IO ()
main = do
    putStrLn "Training data:"
    samples <- trainingData
    sequence_ $ fmap print samples

    let tempSet = normalize $ fmap temp samples
    let seasonSet = normalize $ fmap (seasonNum False . season) samples
    let normalized = zip3 tempSet seasonSet (fmap (boolInput . decision) samples)

    p <- iterateTraining normalized epoch $ biasedSigmoid [(-1), 1]
    putStrLn $ "Final state:" ++ show (weights p) ++ "  BIAS: " ++ show (bias p)
