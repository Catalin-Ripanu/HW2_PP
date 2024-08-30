module AlgebraicGraph where

import qualified Data.Set as S

data AlgebraicGraph a
    = Empty
    | Node a
    | Overlay (AlgebraicGraph a) (AlgebraicGraph a)
    | Connect (AlgebraicGraph a) (AlgebraicGraph a)
    deriving (Eq, Show)

-- (1, 2), (1, 3)
angle :: AlgebraicGraph Int
angle = Connect (Node 1) (Overlay (Node 2) (Node 3))

-- (1, 2), (1, 3), (2, 3)
triangle :: AlgebraicGraph Int
triangle = Connect (Node 1) (Connect (Node 2) (Node 3))

{-
    *** TODO ***

    Mulțimea nodurilor grafului.

    Hint: S.union
-}

nodes :: Ord a => AlgebraicGraph a -> S.Set a
nodes Empty = (S.fromList [])
nodes (Node a) = (S.fromList [a])
nodes (Overlay a b) = (S.union (nodes a) (nodes b))
nodes (Connect a b)= (S.union (nodes a) (nodes b))

{-
    *** TODO ***

    Mulțimea arcelor grafului.

    Hint: S.union, S.cartesianProduct
-}

edges :: Ord a => AlgebraicGraph a -> S.Set (a, a)
edges Empty = (S.fromList [])
edges (Node a) = (S.fromList [(a,a)])
edges (Overlay a b) = (S.filter  (\(x, y) -> x /= y) (S.union (edges a) (edges b)))
edges (Connect a b) = (S.filter  (\(x, y) -> x /= y) (S.union (S.cartesianProduct (nodes a) (nodes b)) (S.union (edges a) (edges b))))
--edges graph = undefined

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
outNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
outNeighbors node Empty = (S.fromList [])
outNeighbors node (Node a) = (S.fromList [])
outNeighbors node (Overlay a b) = (S.union (outNeighbors node a) (outNeighbors node b))
outNeighbors node (Connect a b) 
                                | (node `S.member` (nodes a)) || (node `S.member` (nodes b)) = (if (node `S.member` (nodes a)) then (nodes b) else (outNeighbors node b))
                                | otherwise = (S.fromList [])
      --  (if ((head (S.toList (nodes a))) == node) || ((head (S.toList (nodes b))) == node) then 
      --  (if ((head (S.toList (nodes a))) == node) then (nodes a) else (nodes b)))

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    ATENȚIE! NU folosiți funcția edges definită mai sus, pentru că ar genera
    prea multe muchii inutile.
-}
inNeighbors :: Ord a => a -> AlgebraicGraph a -> S.Set a
inNeighbors node Empty = (S.fromList [])
inNeighbors node (Node a) = (S.fromList [])
inNeighbors node (Overlay a b) = (S.union (inNeighbors node a) (inNeighbors node b))
inNeighbors node (Connect a b) 
                                | (node `S.member` (nodes a)) || (node `S.member` (nodes b)) = (if (node `S.member` (nodes b)) then (S.union (inNeighbors node b) (nodes a)) else (inNeighbors node a))
                                | otherwise = (S.fromList [])

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, se întoarce același graf.

    Hint: Definiți o funcție recursivă locală (de exemplu, în where),
    care să primească drept parametri doar entități variabile de la un apel
    recursiv la altul, astfel încât să nu copiați la fiecare apel parametrii
    nemodificați. De exemplu, parametrul node nu se modifică, în timp ce
    parametrul graph se modifică.
-}

fromM :: (Eq a ) => Maybe a -> a -> a
fromM y x 
        | y == Nothing = x
        | otherwise = (\(Just h) -> h) y

buildg f = f Empty Node Overlay Connect

removeNode :: Eq a => a -> AlgebraicGraph a -> AlgebraicGraph a
removeNode node graph 
                    | graph == Empty = graph
                    | otherwise = func graph
                    where
                         func Empty = graph
                         func (Node a) = (if (Node node) == (Node a) then Empty else (Node a))
                         func (Overlay a b) = (Overlay (func a) (func b))
                         func (Connect a b) = (Connect (func a) (func b))
     

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.
    
    Hint: Funcție recursivă locală, ca la removeNode.
-}
splitNode :: Eq a
          => a                 -- nodul divizat
          -> [a]               -- nodurile cu care este înlocuit
          -> AlgebraicGraph a  -- graful existent
          -> AlgebraicGraph a  -- graful obținut
splitNode old news graph 
                        | graph == Empty = graph
                        | otherwise = (removeNode old (func1 graph news))
                    where
                         func1 Empty news = graph
                         func1 (Node a) news = (if ((Node old) == (Node a) && news /= []) then ((func1 (Overlay (Node (head news)) (Node old)) (tail news))) else (Node a))
                         func1 (Overlay a b) news = (Overlay (func1 a news) (func1 b news))
                         func1 (Connect a b) news = (Connect (func1 a news) (func1 b news))
{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Hint: Funcție recursivă locală, ca la removeNode.
-}
mergeNodes :: (a -> Bool)       -- proprietatea îndeplinită de nodurile îmbinate
           -> a                 -- noul nod
           -> AlgebraicGraph a  -- graful existent
           -> AlgebraicGraph a  -- graful obținut
mergeNodes prop node graph = case graph of
                            Empty -> graph
                            _ -> (func2 graph)
                    where
                         func2 Empty = graph
                         func2 (Node a) = (if (prop a) then (Node node) else (Node a))
                         func2 (Overlay a b) = (Overlay (func2 a) (func2 b))
                         func2 (Connect a b) = (Connect (func2 a) (func2 b))
