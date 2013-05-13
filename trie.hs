data Trie a = 
	Trie { value :: Maybe a,
			children ::[(Char, Trie a)]}
			
			
find :: String -> Trie a -> Maybe a
find [] t = value t
find (k:ks) t = case lookup k (children t) of
				Nothing -> Nothing
				Just ct -> find ks ct
				
				
makeTrie :: [String] -> Trie a
makeTrie words = insert (Trie Nothing []) words

insert :: [String] -> Trie a -> Trie a
insert trie@(Trie _ children) (item:items) = addItem trie item :: children