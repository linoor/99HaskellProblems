import Data.List
import Control.Monad

solveRPN :: String -> Maybe Double
solveRPN expression = do
			[result] <- foldM foldingFunction [] (words expression)
			return result
	where
		foldingFunction :: [Double] -> String -> Maybe [Double]
		foldingFunction (x:y:ys) "*"    = return $ (x*y):ys
		foldingFunction (x:y:ys) "+"    = return $ (x+y):ys
		foldingFunction (x:y:ys) "-"    = return $ (y-x):ys
		foldingFunction (x:y:ys) "/"    = return $ (y/x):ys
		foldingFunction (x:y:ys) "^"    = return $ (y**x):ys
		foldingFunction (x:xs) "ln"     = return $ log x:xs
		foldingFunction xs "sum"        = return $ [sum xs]
		foldingFunction xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of
				[(x, "")] -> Just x
				_         -> Nothing
