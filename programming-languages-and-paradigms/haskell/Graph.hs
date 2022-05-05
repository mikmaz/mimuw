{- |
Author   : Mikołaj Mazurczyk
Index No : 426819
-}

module Graph where
import Set(Set)
import qualified Set as Set
import qualified Data.List

{- |
#BEGIN# AUXILIARY FUNCTIONS
-}

relationFromBasic :: Basic a -> Relation a
relationFromBasic = fromBasic

{- | 'toListBasic' @g returns representation (@vs@, @es@) of Basic graph @g@ where @vs@
is list of it's vertices and @es@ is list of pairs (@v1@, @v2@). Each pair from @es@
represents edge 'v1 -> v2'. Returned lists are sorted and they don't contain duplicates.
 -}
toListBasic :: Ord a => Basic a -> ([a], [(a, a)])
toListBasic Empty = ([],[])
toListBasic (Vertex v) = ([v], [])
toListBasic (Union g1 g2) = (vAccUnion, eAccUnion) where
    (vAcc1, eAcc1) = toListBasic g1
    (vAcc2, eAcc2) = toListBasic g2
    vAccUnion = Set.nubSorted (Data.List.sort (vAcc1 ++ vAcc2))
    eAccUnion = Set.nubSorted (Data.List.sort (eAcc1 ++ eAcc2))
toListBasic (Connect g1 g2) = (vAccUnion, eAccUnion) where
    (vAcc1, eAcc1) = toListBasic g1
    (vAcc2, eAcc2) = toListBasic g2
    newEdges = Set.nubSorted (Data.List.sort [(v1, v2) | v1 <- vAcc1, v2 <- vAcc2])
    vAccUnion = Set.nubSorted (Data.List.sort (vAcc1 ++ vAcc2))
    eAccUnion = Set.nubSorted (Data.List.sort (eAcc1 ++ eAcc2 ++ newEdges))

notInEdge :: Eq a => a -> (a, a) -> Bool
notInEdge v (v1, v2) = (v /= v1) && (v /= v2)

{- |
AUXILIARY FUNCTIONS #END#
-}

class Graph g where
  empty   :: g a
  vertex  :: a -> g a
  union   :: g a -> g a -> g a
  connect :: g a -> g a -> g a

data Relation a = Relation { domain :: Set a, relation :: Set (a, a) }
    deriving (Eq, Show)

data Basic a = Empty
             | Vertex a
             | Union (Basic a) (Basic a)
             | Connect (Basic a) (Basic a)

instance Graph Relation where
    empty = Relation Set.empty Set.empty

    vertex v = Relation (Set.singleton v) Set.empty

    union r1 r2 = Relation (Set.union (domain r1) (domain r2)) 
                           (Set.union (relation r1) (relation r2))

    connect Relation {domain = Set.Empty, relation = Set.Empty} r2 = r2
    connect r1 Relation {domain = Set.Empty, relation = Set.Empty} = r1
    connect r1 r2 = Relation newDomain newRelations where
        vs1 = Set.toList (domain r1)
        vs2 = Set.toList (domain r2)
        newEdges = Set.fromList [(v1, v2) | v1 <- vs1, v2 <- vs2]
        newRelations = Set.union (Set.union (relation r1) (relation r2)) newEdges
        newDomain = Set.union (domain r1) (domain r2)
                
instance (Ord a, Num a) => Num (Relation a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Graph Basic where
    empty = Empty

    vertex v = Vertex v

    union g Empty = g
    union Empty g = g
    union g1 g2 = Union g1 g2

    connect g Empty = g
    connect Empty g = g
    connect g1 g2 = Connect g1 g2

instance Ord a => Eq (Basic a) where
    g1 == g2 = relationFromBasic g1 == relationFromBasic g2

instance (Ord a, Num a) => Num (Basic a) where
    fromInteger = vertex . fromInteger
    (+)         = union
    (*)         = connect
    signum      = const empty
    abs         = id
    negate      = id

instance Semigroup (Basic a) where
  (<>) = union

instance Monoid (Basic a) where
  mempty = Empty

fromBasic :: Graph g => Basic a -> g a
fromBasic Empty = empty
fromBasic (Vertex v) = vertex v
fromBasic (Union g1 g2) = fromBasic g1 `union` fromBasic g2
fromBasic (Connect g1 g2) = fromBasic g1 `connect` fromBasic g2

instance (Ord a, Show a) => Show (Basic a) where
    show Empty = "edges [] + vertices []"
    show g = "edges " ++ show finalEdges ++ " + vertices " ++ show finalVertices  where
        (unfilteredVs, finalEdges) = toListBasic g
        finalVertices = filter (\v -> all (notInEdge v) finalEdges) unfilteredVs

-- | Example graph
-- >>> example34
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

example34 :: Basic Int
example34 = 1*2 + 2*(3+4) + (3+4)*5 + 17

todot :: (Ord a, Show a) => Basic a -> String
todot g = "digraph {\n" ++ 
          edgesToString finalEdges ++ 
          verticesToString finalVertices ++ 
          "}"  
  where 
    (unfilteredVs, finalEdges) = toListBasic g
    finalVertices = filter (\v -> all (notInEdge v) finalEdges) unfilteredVs

    verticesToString :: Show a => [a] -> String
    verticesToString [] = ""
    verticesToString (v:vs) = show v ++ ";\n" ++ verticesToString vs

    edgesToString :: Show a => [(a, a)] -> String
    edgesToString [] = ""
    edgesToString ((v1, v2):vs) = show v1 ++ " -> " ++ show v2 ++ ";\n" ++ 
                                  edgesToString vs

instance Functor Basic where
    fmap f Empty = empty
    fmap f (Vertex v) = vertex (f v)
    fmap f (Connect v1 v2) = connect (fmap f v1) (fmap f v2)
    fmap f (Union g1 g2) = fmap f g1 `union` fmap f g2

-- | Merge vertices
-- >>> mergeV 3 4 34 example34
-- edges [(1,2),(2,34),(34,5)] + vertices [17]

mergeV :: Eq a => a -> a -> a -> Basic a -> Basic a
mergeV a b c = fmap (transform a b c) where
    transform :: Eq a => a -> a -> a -> a -> a
    transform a b c v = if v == a || v == b then c else v

instance Applicative Basic where
    pure = Vertex
    (Vertex f) <*> b = fmap f b
    (Union f g) <*> b = union (f <*> b) (g <*> b)
    (Connect f g) <*> b = connect (f <*> b) (g <*> b)

instance Monad Basic where
    return = Vertex
    Empty >>= _ = empty
    (Vertex v) >>= f = f v
    (Union g1 g2) >>= f = (g1 >>= f) `union` (g2 >>= f)
    (Connect g1 g2) >>= f = connect (g1 >>= f) (g2 >>= f)

-- | Split Vertex
-- >>> splitV 34 3 4 (mergeV 3 4 34 example34)
-- edges [(1,2),(2,3),(2,4),(3,5),(4,5)] + vertices [17]

splitV :: Eq a => a -> a -> a -> Basic a -> Basic a
splitV a b c g = Union (Vertex (transform a b)) (Vertex (transform a c)) <*> g where
    transform :: Eq a => a -> a -> a -> a
    transform a b v = if a == v
                          then b
                          else v
