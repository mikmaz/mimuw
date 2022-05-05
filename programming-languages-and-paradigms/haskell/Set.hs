{- |
Author   : Mikołaj Mazurczyk
Index No : 426819
-}

module Set(Set(..), empty, null, singleton, union, fromList
              , member, toList, toAscList, elems, nubSorted
              ) where
import Prelude hiding(null)
import qualified Data.List

-- | 'nubSorted' @xs removes duplicates from sorted list @xs@.
nubSorted :: Eq a => [a] -> [a]
nubSorted xs = nubSortedAcc (reverse xs) [] where
    nubSortedAcc :: Eq a => [a] -> [a] -> [a]
    nubSortedAcc [] acc = acc
    nubSortedAcc [x] acc = x : acc
    nubSortedAcc (x : y : xs) acc = if x == y
                                        then nubSortedAcc (y:xs) acc
                                        else nubSortedAcc (y:xs) (x:acc)

data Set a = Empty
           | Singleton a
           | Union (Set a) (Set a)

empty :: Set a
empty = Empty

null :: Set a -> Bool
null Empty = True
null _ = False

member :: Eq a => a -> Set a -> Bool
member _ Empty = False
member x (Singleton y) = x == y
member x (Union s1 s2) = member x s1 || member x s2

singleton :: a -> Set a
singleton = Singleton

fromList :: [a] -> Set a
fromList [] = Empty
fromList (x:xs) = if Data.List.null xs
                    then singleton x
                    else singleton x `union` fromList xs

toList :: Set a -> [a]
toList Empty = []
toList (Singleton x) = [x]
toList s = toListAcc s [] where
    toListAcc :: Set a -> [a] -> [a]
    toListAcc (Singleton x) acc = x : acc
    toListAcc (Union s1 s2) acc = toListAcc s2 (toListAcc s1 acc)

toAscList :: Ord a => Set a -> [a]
toAscList s = nubSorted $ Data.List.sort (toList s)

elems :: Set a -> [a]
elems = toList

union :: Set a -> Set a -> Set a
union Empty s2 = s2
union s1 Empty = s1
union s1 s2 = Union s1 s2

insert :: a -> Set a -> Set a
insert x Empty = singleton x
insert x (Singleton y) = singleton x `union` singleton y
insert x s = singleton x `union` s

instance Ord a => Eq (Set a) where
    s1 == s2 = toAscList s1 == toAscList s2

instance Semigroup (Set a) where
    s1 <> s2 = s1 `union` s2

instance Monoid (Set a) where
    mempty = Empty

instance Show a => Show (Set a) where
    show Empty = "Empty"
    show s = "{" ++ setElems ++ "}" where
        setElems = init $ init (showSetElems s)

        showSetElems :: Show a => Set a -> String
        showSetElems (Singleton x) = show x ++ ", "
        showSetElems (Union s1 s2) = showSetElems s1 ++ showSetElems s2

instance Functor Set where
    fmap f Empty = Empty
    fmap f (Singleton x) = Singleton (f x)
    fmap f (Union s1 s2) = fmap f s1 `union` fmap f s2
