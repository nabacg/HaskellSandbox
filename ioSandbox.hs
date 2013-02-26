import Control.Monad
import Data.Char
import System.IO

mainAlt = do
  return ()
  return "HAAHDas"
  --putStrLn "Hello, what's your name?"
  name <- getLine

  return 4
--  putStrLn $ "Read this carefully " ++ name ++  "!"

  c <- getChar
  when (c /= ' ') $ do
    putChar c
    mainAlt

mainColors = do
  colors <- forM [1..4] (\a -> do
                            putStrLn $ "Which color do you associate with: " ++ show a
                            color <- getLine
                            return color)
  putStrLn "Your color mapping"
  mapM putStrLn colors

mainContents = do
  contents <- getContents
  putStr (map toUpper contents)

mainInteract = interact $ unlines . filter ((<10) . length) . lines


mainFile = do
  handle <- openFile "morseData" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle

mainWithFile = do
  withFile "morseData" ReadMode (\handle -> do
                                    contents <- hGetContents handle
                                    putStr (map toUpper contents))


main = do
  contents <- readFile "testFile.txt"
  putStr $ map toUpper contents
  writeFile "testFileCAPS.txt" (map toUpper contents)