{-# LANGUAGE TupleSections #-}
module StandardGraph where

import qualified Data.Set as S

{-
    Graf ORIENTAT cu noduri de tipul a, reprezentat prin mulțimile (set)
    de noduri și de arce.

    Mulțimile sunt utile pentru că gestionează duplicatele și permit
    testarea egalității a două grafuri fără a ține cont de ordinea nodurilor
    și a arcelor.

    type introduce un sinonim de tip, similar cu typedef din C.
-}
type StandardGraph a = (S.Set a, S.Set (a, a))

{-
    *** TODO ***

    Construiește un graf pe baza listelor de noduri și de arce.

    Hint: S.fromList.

    Constrângerea (Ord a) afirmă că valorile tipului a trebuie să fie
    ordonabile, lucru necesar pentru reprezentarea internă a mulțimilor.
    Este doar un detaliu, cu care nu veți opera explicit în această etapă.
    Veți întâlni această constrângere și în tipurile funcțiilor de mai jos.
-}
fromComponents :: Ord a
               => [a]              -- lista nodurilor
               -> [(a, a)]         -- lista arcelor
               -> StandardGraph a  -- graful construit
fromComponents ns es = (S.fromList ns,S.fromList es)

{-
    *** TODO ***

    Mulțimea nodurilor grafului.
-}
nodes :: StandardGraph a -> S.Set a
nodes = fst 

{-
    *** TODO ***

    Mulțimea arcelor grafului.
-}
edges :: StandardGraph a -> S.Set (a, a)
edges = snd

{-
    Exemple de grafuri
-}
graph1 :: StandardGraph Int
graph1 = fromComponents [1, 2, 3, 4, 5] [(1, 2), (3, 4), (4, 5), (5, 3)]

graph2 :: StandardGraph Int
graph2 = fromComponents [4, 3, 3, 2, 1] [(1, 3), (1, 3), (1, 2)]

graph3 :: StandardGraph Int
graph3 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 3), (1, 3)]

graph4 :: StandardGraph Int
graph4 = fromComponents [1, 2, 3, 4] [(1, 2), (1, 4), (4, 1), (2, 4), (1, 3)]

shouldBeTrue :: Bool
shouldBeTrue = graph1 == graph2

{-
    *** TODO ***

    Mulțimea nodurilor înspre care pleacă arce dinspre nodul curent.

    Exemplu:

    > outNeighbors 1 graph3
    fromList [2,3,4]
-}
outNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
outNeighbors node graph = S.map (\x -> snd x) $ S.filter (\y -> if (fst y) == node then True else False) $ edges graph

{-
    *** TODO ***

    Mulțimea nodurilor dinspre care pleacă arce înspre nodul curent.

    Exemplu:

    > inNeighbors 1 graph3 
    fromList [4]
-}
inNeighbors :: Ord a => a -> StandardGraph a -> S.Set a
inNeighbors node graph = S.map (\x -> fst x) $ S.filter (\y -> if (snd y) == node then True else False) $ edges graph

{-
    *** TODO ***

    Întoarce graful rezultat prin eliminarea unui nod și a arcelor în care
    acesta este implicat. Dacă nodul nu există, întoarce același graf.

    Exemplu:

    > removeNode 1 graph3
    (fromList [2,3,4],fromList [(2,3)])
-}

removeNode :: Ord a => a -> StandardGraph a -> StandardGraph a
removeNode node graph  
      | notElem node $ nodes graph = graph
      | otherwise = fromComponents (S.toList (S.delete node (nodes graph))) 
      (S.toList (S.filter (\x -> if fst x == node || snd x == node then False else True) $ edges graph))

{-
    *** TODO ***

    Divizează un nod în mai multe noduri, cu eliminarea nodului inițial.
    Arcele în care era implicat vechiul nod trebuie să devină valabile
    pentru noile noduri.

    Exemplu:

    > splitNode 2 [5,6] graph3
    (fromList [1,3,4,5,6],fromList [(1,3),(1,4),(1,5),(1,6),(4,1),(5,3),(6,3)])
-}

func :: Ord a => a -> [(a,a)] -> [a] -> [(a,a)]
func x l y = map (\(f,h) -> if f `elem` y then if (h `elem` y) then (x,x) else (x,h) else if (h `elem` y) then (f,x) else (f,h)) l

func1 :: Ord a => [a] -> [(a,a)] -> [a] -> [(a,a)]
func1 x l y = foldr (\x acc -> func x l y ++ acc) [] x

splitNode :: Ord a
          => a                -- nodul divizat
          -> [a]              -- nodurile cu care este înlocuit
          -> StandardGraph a  -- graful existent
          -> StandardGraph a  -- graful obținut
splitNode old news graph = (removeNode old (fromComponents ((S.toList (nodes graph)) ++ news) 
  ((func1 news (S.toList (S.filter (\y -> if (snd y) == old || (fst y) == old then True else False) $ edges graph)) [old]) 
  ++ (S.toList (edges graph)))))

{-
    *** TODO ***

    Îmbină mai multe noduri într-unul singur, pe baza unei proprietăți
    respectate de nodurile îmbinate, cu eliminarea acestora. Arcele în care
    erau implicate vechile noduri vor referi nodul nou.

    Exemplu:

    > mergeNodes even 5 graph3
    (fromList [1,3,5],fromList [(1,3),(1,5),(5,1),(5,3)])
-}

nub' :: Ord a => [a] -> [a]
nub' xs = foldr go (`seq` []) xs S.empty
  where
    go x r obs
      | x `S.member` obs = r obs
      | otherwise = obs' `seq` x : r obs'
      where obs' = x `S.insert` obs

mergeNodes :: Ord a
           => (a -> Bool)      -- proprietatea îndeplinită de nodurile îmbinate
           -> a                -- noul nod
           -> StandardGraph a  -- graful existent
           -> StandardGraph a  -- graful obținut

mergeNodes prop node graph = 
                           let list  =  (S.toList (S.filter (\x-> if prop x == True then False else True) (nodes graph)))
                               list1 =  (S.toList (S.filter (\x-> if prop x == True then True else False) (nodes graph)))
                           in ((fromComponents (if list1 /= [] then (list ++ (node : [])) else list)
  ((nub' (func1 (node : []) (S.toList (edges graph)) list1)))))
