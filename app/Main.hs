module Main where

main :: IO ()
main = interact (unlines . (map solve) . readInput)

readInput :: String -> [Integer]
readInput   = (map read) . words

solve :: Integer -> String
solve x =
  let
    initIteration                   = 0
    (resultPrime, resultItearation) = reducePrimes x initIteration
  in
    show resultPrime ++ " " ++ show resultItearation

reducePrimes :: Integer -> Integer -> (Integer, Integer)
reducePrimes dividend iteration =
  let
    initAcc             = 0
    (oddDividend, acc)  = reduceStatic dividend 2 initAcc
    newIteration        = iteration + 1
  in
    case reduceIncremental oddDividend 3 acc of
      primeResult | primeResult == dividend -> (primeResult, newIteration)
      newDividend                           -> reducePrimes newDividend newIteration

reduceStatic :: Integer -> Integer -> Integer -> (Integer, Integer)
reduceStatic dividend divisor acc =
  case quotRem dividend divisor of
    (newDividend, 0)  -> reduceStatic newDividend divisor (acc + divisor)
    (_, _)            -> (dividend, acc)

reduceIncremental :: Integer -> Integer -> Integer -> Integer
reduceIncremental dividend divisor acc =
  if (fromIntegral divisor) <= (sqrt $ fromIntegral dividend)
    then
      let
        (newDividend, newAcc) = reduceStatic dividend divisor acc
      in
        reduceIncremental newDividend (divisor + 2) newAcc
    else
      if dividend > 2
        then acc + dividend
        else acc
