import Data.List
import Data.Char

main :: IO()
main = putStrLn . show $ problem9


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

problem6 :: Integer
problem6 = ((^2) . sum $ [1..100]) - (sum . map (^2) $ [1..100])

problem7 :: Integer
problem7 = primes !! 10000
  where
    primes = sieve (2 : [3, 5..])
    sieve (p:xs) = p : sieve [x|x <- xs, x `mod` p > 0]

problem8 :: Int
problem8 = maximum . map product . map (\x -> map digitToInt x) . filter (\x -> all (\y -> y /= '0') x ) $ substrings
  where
    digits = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    substrings = foldl (\x y -> (take 13 . drop y $ digits) : (x)) [] [0..987]

problem9 :: Int
problem9 = head . map (\(a, b, c) -> a*b*c) . triplets $ thousands
  where
    triplets = filter (\(a, b, c) -> (square a) + (square b) == (square c))
    thousands = [(a, b, c) | a <- [1..999], b <- [(a+1)..999], c <- [(b+1)..999], a + b + c == 1000]
    square = (^2)
