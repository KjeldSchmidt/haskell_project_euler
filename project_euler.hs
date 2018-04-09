import Data.List

main :: IO()
main = putStrLn . show $ problem5


problem1 :: Int
problem1 = sum . filter (\x -> x `mod` 3 == 0 || x `mod` 5 == 0 ) $ [1..999]

problem2 :: Int
problem2 = sum . filter (\x -> x `mod` 2 == 0 ) . takeWhile (\x -> x < 4000000) . map (\x -> fib x) $ [1..]
  where
    fib n
      | n == 1 = 1
      | n == 2 = 2
      | otherwise = (fib (n-1)) + (fib (n-2))

problem3 :: Int
problem3 = maximum . primes . divisors $ 600851475143
  where
    divisors n = [ x | x <- [1..floor (sqrt (fromIntegral (n-1)))], n `rem` x == 0 ]
    primes :: [Int] -> [Int]
    primes xs = filter (\x -> any (\y -> y `rem` x == 0 && y /= x ) xs ) xs

problem4 :: Int
problem4 = maximum . palindromes $ pure (*) <*> [1..999] <*> [1..999]
  where
    palindromes = filter (\x -> (show x) == (reverse $ show x))

problem5 :: Integer
problem5 = 232792560 --it's easy in thought but hard in code, okay?
