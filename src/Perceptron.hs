module Perceptron (
    Perceptron(..),
    Sigmoid,
    BiasedSigmoid,
    sigmoid,
    biasedSigmoid,
    bias,
    compute,
    errorFunction,
    train
) where


class Perceptron p where
    learningRate :: Floating a => p a -> a
    weights :: p a -> [a]
    activation :: Floating a => p a -> a -> a
    derrivative :: Floating a => p a -> a -> a -- derivative of the activation function
    updateWeights :: Floating a => p a -> a -> [a] -> p a


sumInputs :: (Perceptron p, Num a) => p a -> [a] -> a
sumInputs perceptron inputs =
        sum . zipWith (*) inputs $ weights perceptron

compute :: (Perceptron p, Floating a) => p a -> [a] -> a
compute perceptron inputs =
        activation perceptron $ sumInputs perceptron inputs

absoluteError :: (Perceptron p, Floating a) => p a -> [a] -> a -> a
absoluteError perceptron inputs expectedResult =
            compute perceptron inputs - expectedResult

-- This is in fact an integral of the absolute error of the perceptron.
-- Absolute error is computedResult - expectedResult
-- DerrivativeOfError is proportional to that (f'(x) = x)
-- Therefore errorFunction is f(x) = x**2 / 2 (integral of f'(x) = x)
-- If you don't understand, don't worry, neither do I. As R. Feynman used to
-- say, shut up and count! ;)
errorFunction :: (Perceptron p, Floating a) => p a -> [a] -> a -> a
errorFunction perceptron inputs expectedResult =
        (absoluteError perceptron inputs expectedResult ** 2) / 2

train :: (Perceptron p, Floating a) => p a -> [a] -> a -> (p a, a)
train perceptron inputs expectedResult =
        let absError = absoluteError perceptron inputs expectedResult
            derrivativeOfActivation = derrivative perceptron $ sumInputs perceptron inputs
            derrivativeOfError = absError * derrivativeOfActivation
        in
        (updateWeights perceptron derrivativeOfError inputs, (absError ** 2) / 2)


sigmoidF :: Floating a => a -> a
sigmoidF x = 1 / (1 + exp (-x))

sigmoidF' :: Floating a => a -> a
sigmoidF' x =
        let s = sigmoidF x in
        s * (1 - s)

data Sigmoid a = Sigmoid [a]

data BiasedSigmoid a = BiasedSigmoid a [a]

bias :: BiasedSigmoid a -> a
bias (BiasedSigmoid b _) = b

instance Perceptron Sigmoid where
    learningRate (Sigmoid _) = 0.08
    weights (Sigmoid ws) = ws
    activation (Sigmoid _) = sigmoidF
    derrivative (Sigmoid _) = sigmoidF'
    updateWeights p derrivativeOfError inputs =
        Sigmoid . zipWith (-) (weights p) $
            fmap ((* derrivativeOfError) . (* learningRate p)) inputs

instance Perceptron BiasedSigmoid where
    learningRate (BiasedSigmoid _ _) = 0.08
    weights (BiasedSigmoid _ ws) = ws
    activation (BiasedSigmoid bias _) = sigmoidF . (+ bias)
    derrivative (BiasedSigmoid bias _) = sigmoidF' . (+ bias)
    updateWeights p@(BiasedSigmoid bias ws) derrivativeOfError inputs =
        BiasedSigmoid
            (bias + (derrivativeOfError * learningRate p))
            (zipWith (-) ws $ fmap ((* derrivativeOfError) . (* learningRate p)) inputs)

sigmoid :: [a] -> Sigmoid a
sigmoid = Sigmoid

biasedSigmoid :: Num a => [a] -> BiasedSigmoid a
biasedSigmoid = BiasedSigmoid 0
