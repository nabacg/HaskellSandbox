import Data.Char

data Tree c = Node c (Tree c) (Tree c)  | EmptyNode deriving (Show, Read, Eq)

leaf :: a -> (Tree a)
leaf a = Node a EmptyNode EmptyNode

morseCodesTree =
  let 
      q = leaf 'Q' 
      z = leaf 'Z'
      y = leaf 'Y'
      c = leaf 'C'
      x = leaf 'X'
      b = leaf 'B'
      j = leaf 'J'
      p = leaf 'P'
      l = leaf 'L'
      f = leaf 'F'
      v = leaf 'V'
      h = leaf 'H'
      
      o = leaf 'O'
      g = Node 'G' q z
      k = Node 'K' y c
      d = Node 'D' x b
      w = Node 'W' j p
      r = Node 'R' EmptyNode l
      u = Node 'U' EmptyNode f
      s = Node 'S' v h

      m = Node 'M' o g
      n = Node 'N' k d
      a = Node 'A' w r
      i = Node 'I' u s

      t = Node 'T' m n
      e = Node 'E' a i
  in Node '_' t e

decodeMorse :: Tree Char -> String -> Char
decodeMorse (Node c _ _) [] = c
decodeMorse EmptyNode _ = error "Failed to find code"
decodeMorse  (Node _ left right) (s:ss)
  | s == '-' = decodeMorse left ss
  | s == '.' = decodeMorse right ss

decodeMorseLine :: String -> String
decodeMorseLine  = map (decodeMorse morseCodesTree) . words


main = interact decodeMorseLine 