import Data.List

-- String -> [String] -> [[String]] -> [String] -> String
-- words :: String -> [String]
-- brokenLines :: [String] -> [[String]]
-- wordJoinedLines :: [[String]] -> [String]
-- joinedLines :: [String] -> String

wordWrap :: String -> String
wordWrap = joinedLines . wordJoinedLines . brokenLines . words 

brokenLines :: [String] -> [[String]]
brokenLines [] = []
brokenLines words = firstLine : brokenLines remainingLines
  where (firstLine, remainingLines) = splitAt (brokenLineWordCount words) words


brokenLineWordCount :: [String] -> Int
brokenLineWordCount  = length . takeWhile (< 80) . scanl1 (+) . map length

wordJoinedLines :: [[String]] -> [String]
wordJoinedLines = map (intercalate " ")


joinedLines :: [String] -> String
joinedLines = intercalate "\n"

main = do
  contents <- getContents
  putStr $ wordWrap contents