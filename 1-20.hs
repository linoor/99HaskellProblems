import Data.List

myLast :: [a] -> a
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a
myButLast (x:y:[]) = x
myButLast (x:xs) = myButLast xs

elementAt :: Int -> [a] -> a
elementAt 1 (x:xs) = x
elementAt k (x:xs) = elementAt (k-1) xs

myLength :: [a] -> Int
myLength xs = foldr (\x acc -> acc + 1) 0 xs

myReverse :: [a] -> [a]
myReverse xs = foldl (\acc x -> x:acc) [] xs

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = reverse xs == xs

data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem a)      = [a]
flatten (List [])     = []
flatten (List (x:xs)) = flatten x ++ flatten (List xs)

secondFlatten :: NestedList a -> [a]
secondFlatten (Elem a) = [a]
secondFlatten (List xs) = concatMap flatten xs

compress :: (Eq a) => [a] -> [a]
compress xs = foldr foldingFun [] xs
	where
		foldingFun new acc
					| acc == []        =  addNew
					| head acc == new  =  acc
					| otherwise        =  addNew
			where addNew = new : acc

pack :: (Eq a) => [a] -> [[a]]
pack xs = groupBy (\x y -> x == y) xs

encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = map (\x -> (length x, head x)) $ pack xs

data Amount a = Multiple Int a | Single a deriving (Show)
encodeModified :: (Eq a) => [a] -> [Amount a]
encodeModified xs = map (\x -> let len = length x in if len == 1 then Single (head x) else Multiple len (head x)) $ pack xs

decodeModified :: [Amount a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decode x ++ decodeModified xs
	where
		decode (Single a)     = [a]
		decode (Multiple n a) = replicate n a

encodeDirect :: (Eq a) => [a] -> [Amount a]
encodeDirect xs = foldr foldFun [] xs
	where
		foldFun new [] = [Single new]
		foldFun new acc = case (head acc) of
					(Single c) -> if c == new then (Multiple 2 c) : (tail acc) else (Single new) : acc
					(Multiple n c) -> if c == new then (Multiple (n+1) c) : (tail acc) else (Single new) : acc
			
