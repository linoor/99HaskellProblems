import Data.List

data BookInfo = Book Int String [String]
				deriving (Show)

data MagazineInfo = Magazine Int String [String]
					deriving (Show)

type CustomerID = Int
type ReviewBody = String
data BetterReview = BetterReview BookInfo CustomerID ReviewBody

type CardHolder = String
type CardNumber = String
type Address = [String]

data BillingInfo = CreditCard CardNumber CardHolder Address
				| CashOnDelivery
				| Invoice CustomerID
				deriving (Show)

data Cartesian2D = Cartesian2D Double Double
				deriving (Eq, Show)

data Polar2D = Polar2D Double Double
	deriving (Eq, Show)

data Colors = Red | Orange | Yellow
			deriving (Eq, Show)

data Customer = Customer {
	customerID :: CustomerID
,	customerName :: String
,	customerAddress :: Address
} deriving (Show)

data List a = Cons a (List a)
			| Nil
			deriving (Show)

toList Nil = []
toList (Cons x xs) = x : toList xs

fromList (x:xs) = Cons x (fromList xs)
fromList []		= Nil

safeSecond :: [a] -> Maybe a
safeSecond [] = Nothing
safeSecond xs = if null (tail xs)
				then Nothing
				else Just (head (tail xs))

myMean xs = mySum xs / (fromIntegral $ length xs)
	where
		mySum xs = foldr (+) 0 xs

intoPalindrome xs = xs ++ reverse xs

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

sortBySublistLength :: [[a]] -> [[a]]
sortBySublistLength xs = sortBy (\a b -> if length a > length b then LT else GT) xs

myIntersperse :: a -> [[a]] -> [a]
myIntersperse sep (x:[]) = x
myIntersperse sep (x:xs) = x ++ sep : (myIntersperse sep xs)

data Tree a = Node a (Tree a) (Tree a)
			| Empty
			deriving (Show)

treeHeight :: Tree a -> Int
treeHeight Empty = 0
treeHeight (Node parent leftChild rigthChild) = 1 + (maximum $ ([treeHeight leftChild, treeHeight rigthChild]))

data Direction = Left | Right | Straight deriving (Show)
