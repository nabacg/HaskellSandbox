
solveRPN :: String -> Float
solveRPN =  head. foldl foldingFunction [] . words
            where foldingFunction (x:y:xs) "*" = (x * y) : xs
                  foldingFunction (x:y:xs) "+" = (x + y) : xs
                  foldingFunction (x:y:xs) "-" = (y - x) : xs
                  foldingFunction (x:y:xs) "/" = (y / x) :xs
                  foldingFunction (x:y:xs) "^" = (y ** x) : xs
                  foldingFunction (x:xs) "ln"  = log x : xs
                  foldingFunction xs "sum"     =  [sum xs]
                  foldingFunction items  numberString = read numberString : items




main = do
  let expr = "90 34 12 33 55 66 + * - + -" 
      expr2 ="10 4 3 + 2 * -"
  putStrLn expr
  putStrLn $ show $ solveRPN expr -- expect 4037
  putStrLn expr2
  putStrLn $ show $ solveRPN expr2  -- expect -4