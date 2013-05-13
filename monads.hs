import Control.Monad
import Control.Monad.Writer

-- instance Monad [] where
--   return x = [x]
--   xs >>= f = concat (map f xs)
--   fail _ = []

--putStrLn $ show [1..4] >>= \x -> [x, 10 - x, -x]
--  putStrLn $ show [1..2] >>= \x -> ['a', 'z'] >>= \y -> return(x, y)

listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1..4]
  ch <- ['a'..'z']
  return (n, ch)


type KnightPos = (Int, Int)

moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, v) = do
  (c', v') <- [(c+2, v+1), (c+2, v-1), (c-2, v+1), (c-2, v-1),
              (c+1, v+2), (c+1, v-1), (c-1, v+2), (c-1, v-2)]
  guard(c' `elem` [1..8] && v' `elem` [1..8])
  return (c', v')

in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start dest = dest `elem` in3 start

gcd' :: Int -> Int -> Writer [String] Int
gcd' a b =
  | b == 0 = do
    tell ["Finished with " ++ show a]
    return a
  | otherwise = do
    tell [show a ++ " mod " ++ show b ++ " = "show (a `mod` b)]
    gcd' b (a `mod` b)