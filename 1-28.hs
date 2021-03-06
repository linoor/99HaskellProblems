import Data.List
import System.Random 
import Control.Applicative
import Data.Tree hiding (Tree, flatten)

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
decodeModified []     = []
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

dupli :: [a] -> [a]
dupli []     = []
dupli (x:xs) = [x, x] ++ dupli xs

addOneElem :: (Eq a) => [a] -> [a]
addOneElem [] = []
addOneElem xs = (head xs) : foldr foldFun [] xs
	where
		foldFun new [] = [new]
		foldFun new acc
				| new == head acc  =  new : acc
				| otherwise        =  new : (head acc) : acc

repli :: (Eq a) => [a] -> Int -> [a]
repli xs 1 = xs
repli xs n = repli (addOneElem xs) (n-1)

dropEvery :: [a] -> Int -> [a]
dropEvery xs n = helper n xs
	where
		helper _ []     = []
		helper 1 (x:xs) = helper n xs
		helper n (x:xs) = x : (helper (n-1) xs)

mySplit :: [a] -> Int -> ([a], [a])
mySplit xs n = helper ([], xs) n
	where
		helper (left, right) 0 = (left, right)
		helper (left, (r:rs)) n = helper (left ++ [r], rs) (n-1)

slice :: [a] -> Int -> Int -> [a]
slice xs l r = helper xs l (r-l+1)
	where
		helper (x:xs) 1 1 = [x]
		helper (x:xs) 1 r = x : (helper xs) 1 (r-1)
		helper (x:xs) l r = helper xs (l-1) r

rotate :: [a] -> Int -> [a]
rotate xs n = let (left, right) = mySplit xs placeToSplit in right ++ left
	where
		placeToSplit = if n >= 0 then n else (length xs) - (abs n)

removeAt :: Int -> [a] -> (a, [a])
removeAt 1 (x:xs) = (x, xs)
removeAt n (x:xs) = let (r, rs) = removeAt (n-1) xs in (r, x:rs)

insertAt :: a -> [a] -> Int -> [a]
insertAt elem xs 1 = elem : xs
insertAt elem (x:xs) n = x : (insertAt elem xs (n-1))

range :: Int -> Int -> [Int]
range left right = helper left
	where
		helper current
			| current > right = [] 
			| current < left = nextNum
			| current >= left && current <= right = current : nextNum
			where
				nextNum = helper (current+1)

randomSelect :: [a] -> Int -> IO [a]
randomSelect xs n = do
			gen <- newStdGen
			let generatedIndexes = take n $ randomRs (0, (length xs)-1) gen
			return $ [xs !! x | x <- generatedIndexes] 

isPrime :: Int -> Bool
isPrime n
	| n <= 1                            =  False
	| n <= 3                            =  True
	| n `mod` 2 == 0 || n `mod` 3 == 0  =  False
	| otherwise                         =  helper 5
	where 
		helper i
			| i*i > n                                 =  True
			| n `mod` i == 0 || n `mod` (i + 2) == 0  =  False
			| otherwise                               =  helper (i+6)

myGCD :: Int -> Int -> Int
myGCD a b
      | b == 0     = abs a
      | otherwise  = myGCD b (a `mod` b)

coprime :: Int -> Int -> Bool
coprime a b = myGCD a b == 1

totient :: Int -> Int
totient 1 = 1
totient n = length $ filter (coprime n) [1..n-1]

primeFactors :: Int -> [Int]
primeFactors 1 = []
primeFactors n = primeFactors' 2 where
	primeFactors' x
		| x == n          = [x]
		| n `mod` x == 0  = x : primeFactors (n `div` x)
		| otherwise       = primeFactors' (x+1)

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult n = map encode . group $ primeFactors n
	where encode xs = (head xs, length xs)

totientImproved :: Int -> Int
totientImproved 1 = 1
totientImproved n = foldl foldFun 1 $ primeFactorsMult n
	where
		foldFun acc new = let (p, m) = new in ((p-1) * p ^ (m-1)) * acc

data SieveMark = Marked | UnMarked | Prime deriving (Show, Eq)
primes :: Int -> [Int]
primes limit = helper allNums where
	helper :: [(SieveMark, Int)] -> [Int]
	helper xs = case potentialNum of
		Just (mark, p) -> helper $ markAllMultiples p xs
		Nothing        -> map snd $ filter (\(mark, num) -> mark == Prime) xs
		where
			potentialNum = find (\(mark, num) -> mark == UnMarked) xs 
	allNums = map (\x -> (UnMarked, x)) [2..limit]

markAllMultiples :: Int -> [(SieveMark, Int)] -> [(SieveMark, Int)]
markAllMultiples p xs = map markHelper xs where
	markHelper :: (SieveMark, Int) -> (SieveMark, Int)
	markHelper (mark, num)
				| num == p          = (Prime, num)
				| mark == Marked    = (Marked, num)
				| num `mod` p == 0  = (Marked, num)
				| otherwise         = (mark, num)

primesR :: Int -> Int -> [Int]
primesR l r = filter isPrime [l..r]

goldbach :: Int -> Maybe (Int, Int)
goldbach n = find (\(a,b) -> a + b == n) $ (,) <$> primes <*> primes where
	primes = primesR 2 n

goldbachlist :: Int -> Int -> [(Int, Int, Int)]
goldbachlist l r = map (\n -> case goldbach n of
						Just (a,b) -> (n, a, b)
						otherwise -> (-1, -1, -1)
						) nums where
							nums = let startingNum = if l `rem` 2 == 0 then l else l+1 in [startingNum, startingNum+2..r]

goldbachlist' :: Int -> Int -> Int -> [(Int, Int, Int)]
goldbachlist' l r limit = filter (\(n, a, b) -> a > limit && b > limit) $ goldbachlist l r 

gray :: Int -> [String]
gray 0 = [""]
gray n = let xs = gray (n-1) in (map ('0':) xs) ++ (map ('1':) (reverse xs))

data Tree a = Empty | Branch a (Tree a) (Tree a) | Leaf a
	deriving (Show, Eq, Ord)

leaf :: a -> Tree a
leaf x = Branch x Empty Empty

toDataTree (Leaf a) = Node (show a) []
toDataTree (Branch b cs ds) = Node (show b) [toDataTree cs, toDataTree ds]	

printTree :: Show a => Tree a -> IO ()
printTree tree = putStrLn . drawTree $ toDataTree tree
