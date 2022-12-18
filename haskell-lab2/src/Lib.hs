module Lib where

import GHC.Conc.Sync (numCapabilities, par, pseq)

riemannBlock :: Double -> Double -> (Double -> Double) -> Double
riemannBlock intA intB f
  | intA > intB = riemannBlock intB intA f
  | otherwise = (f intA + f intB) / 2.0 * (intB - intA)

riemannIteratorPar :: [Double] -> (Double -> Double) -> Bool -> Int -> [Double]
riemannIteratorPar [_] _ _ _ = []
riemannIteratorPar (p : q : ps) f paral num
  | paral && num <= numCapabilities = tailR `par` (forceElem headR `seq` (headR : tailR))
  | otherwise = headR : tailR
  where
    headR = riemannBlock p q f
    tailR = riemannIteratorPar (q : ps) f paral (num + 1)

forceElem :: a -> ()
forceElem x = x `pseq` ()

riemannSum :: [Double] -> (Double -> Double) -> Bool -> Double
riemannSum partition f paral = sum (riemannIteratorPar partition f paral 1)

createPartition :: (Double, Double) -> Double -> [Double]
createPartition (intA, intB) n
  | intA > intB = createPartition (intB, intA) n
  | otherwise = [intA, intA + (intB - intA) / n .. intB]

integralIterator :: Double -> Double -> (Double, Double) -> (Double -> Double) -> Double -> Bool -> Double
integralIterator lastInt lastN (intA, intB) f prec paral
  | prec > abs (lastInt - currentInt) = currentInt
  | otherwise = integralIterator currentInt currentN (intA, intB) f prec paral
  where
    currentN = lastN * 2
    currentInt = riemannSum (createPartition (intA, intB) currentN) f paral

integral :: (Double, Double) -> (Double -> Double) -> Bool -> Double -> Double
integral (intA, intB) f paral prec = integralIterator (riemannSum [intA, intB] f paral) 1 (intA, intB) f prec paral
