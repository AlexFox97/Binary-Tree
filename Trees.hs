--Import List module so I can used splitAt
import Data.List

--A data type which can be an empty tree, a branch with 2 sub-trees or
-- a leaf.
data Tree t = Branch (Tree t) (Tree t) | Leaf t | EmptyTree deriving Show

--Binary tree generator that takes 2 numbers  and returns a balanced tree
fromToT :: Enum a => a -> a -> Tree a
fromToT m n = generateTree [m..n]

--A helper generator that takes a list instead of numbers
generateTree :: [a] -> Tree a
generateTree [] = EmptyTree
generateTree [x] = Leaf x
generateTree [x,y] = Branch (Leaf x) (Leaf y)
generateTree x = Branch (generateTree firstHalf) (generateTree secondHalf)
                  where firstHalf = fst $ splitAt (div (length x) 2) x
                        secondHalf = snd $ splitAt (div (length x) 2) x

--A tree fold function that folds over the tree without accessing the values
countTreeFold :: (t1 -> t1 -> t1 -> t1) -> t1 -> Tree t2 -> t1
countTreeFold f acc (EmptyTree) = acc
countTreeFold f acc (Leaf t) = acc
countTreeFold f acc (Branch left right) = f (countTreeFold f acc left) (countTreeFold f acc right) acc

--A tree fold function that folds over the tree and accesses the values
accessTreeFold :: ([a] -> [a] -> t -> [a]) -> t -> Tree a -> [a]
accessTreeFold f acc (EmptyTree) = []
accessTreeFold f acc (Leaf t) = [t]
accessTreeFold f acc (Branch left right) = f (accessTreeFold f acc left) (accessTreeFold f acc right) acc

--Size function using higher-order function
size t = countTreeFold (\left right acc -> acc + left + right) 1 t

--Size function without using higher-order functions
size' :: Num a => Tree t -> a
size' EmptyTree = 0
size' (Leaf l) = 1
size' (Branch x y) = 1 + size' x + size' y

--Height function using higher-order functions
height t = countTreeFold (\left right acc -> 1 + max left right ) 0 t

--Height function without higher-order functions
height' :: (Num p, Ord p) => Tree t -> p
height' EmptyTree = 0
height' (Leaf l) = 0
height' (Branch x y) = 1 + max (height' x) (height' y)

--Flattens a tree into a list, uses higher-order functions
flatten t = accessTreeFold (\left right acc -> left ++ right) [] t

--Flattens a tree into a list without using higher-functions
flatten' :: Tree a -> [a]
flatten' EmptyTree = []
flatten' (Leaf l) = [l]
flatten' (Branch x y) = flatten' x ++ flatten' y
