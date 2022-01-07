-- CptS 355 - Fall 2021 -- Homework2 - Haskell
-- Name: Khoa Bui - 11685409
-- Collaborators: 
module HW2
     where

{- 1. groupbyNTail - 10%-}
groupbyNTail iL n = groupHelper iL n n [] []
                    where
                         groupHelper [] m n buf ls = reverse((reverse buf) : ls)
                         groupHelper (x:xs) m n buf ls | (m == 0) = groupHelper (x:xs) n n [] ((reverse buf) : ls)
                                                       | otherwise = groupHelper xs (m-1) n (x:buf) ls
-----------------------------------------------------------

{- 2.  elemAll and stopsAt  -  20% -}

{- (a)  elemAll - 10%-}
-- please don't include the myCatsLog list in your solution file. 
elemAll l1 [] = False
               -- if any elem from l1 has been filtered out -> false
elemAll l1 l2 | length(filter isElem l1) < length(l1) = False
              | otherwise = True
               where
                    isElem x | (x `elem` l2) = True
                             | otherwise = False



{- (b) stopsAt - 10%-}
--buses=[("Wheat",["Chinook","Orchard","Valley","Maple","Aspen","TerreView","Clay","Dismores","Martin","Bishop","Walmart","PorchLight","Campus"]),("Silver",["TransferStation","PorchLight","Stadium","Bishop","Walmart","Shopco","RockeyWay"]),("Blue",["TransferStation","State","Larry","TerreView","Grand","TacoBell","Chinook","Library"]),("Gray",["TransferStation","Wawawai","Main","Sunnyside","Crestview","CityHall","Stadium","Colorado"])]

stopsAt [] routes = []
stopsAt stops routes = map getBuses (filter findStops routes)
                    where
                         findStops (x,y) = elemAll stops y
                         getBuses (x,y) = x

-----------------------------------------------------------

{- 3. isBigger and applyRange - 25% -}

--define the Timestamp datatype
data Timestamp =  DATE (Int,Int,Int) |  DATETIME (Int,Int,Int,Int,Int) 
                  deriving (Show, Eq)

{- (a)  isBigger - 15% -}
isBigger (DATE (m1,d1,y1)) (DATE (m2,d2,y2)) | (y1 > y2) = True
                                             | (y2 > y1) = False
                                             | (m1 > m2) = True
                                             | (m2 > m1) = False
                                             | (d1 > d2) = True
                                             | otherwise = False
isBigger (DATETIME (m1,d1,y1,h1,min1)) (DATETIME (m2,d2,y2,h2,min2))  | (y1 > y2) = True
                                                                      | (y2 > y1) = False
                                                                      | (m1 > m2) = True
                                                                      | (m2 > m1) = False
                                                                      | (d1 > d2) = True
                                                                      | (d2 > d1) = False
                                                                      | (h1 > h2) = True
                                                                      | (h2 > h1) = False
                                                                      | (min1 > min2) = True
                                                                      | otherwise = False
isBigger (DATE (m1,d1,y1)) (DATETIME (m2,d2,y2,h2,min2))    | (y1 > y2) = True
                                                            | (y2 > y1) = False
                                                            | (m1 > m2) = True
                                                            | (m2 > m1) = False
                                                            | (d1 > d2) = True
                                                            -- | (d2 > d1) = False
                                                            | otherwise = False
isBigger (DATETIME (m1,d1,y1,h1,min1)) (DATE (m2,d2,y2))    | (y1 > y2) = True
                                                            | (y2 > y1) = False
                                                            | (m1 > m2) = True
                                                            | (m2 > m1) = False
                                                            | (d1 > d2) = True
                                                            | otherwise = False

{- (b) applyRange - 10% -}
--datelist=[DATE(5,28,2021),DATETIME(6,1,2021,14,15),DATE(6,22,2021),DATE(6,1,2021),DATETIME(6,21,2021,15,20),DATETIME(5,21,2020,14,40),DATE(5,20,2021),DATETIME(6,9,2021,19,30),DATETIME(6,10,2021,11,10)]

applyRange (min, max) ls = filter getRange ls
                         where
                              getRange date | (isBigger date min) && (isBigger max date) = True
                                            | otherwise = False

-----------------------------------------------------------
{-4 - foldTree, createRTree, fastSearch  - 35%-}

--define Tree and RTree data types
data Tree a = LEAF a | NODE a (Tree a) (Tree a)
               deriving (Show,  Eq, Ord)

data RTree a = RLEAF a | RNODE a (a,a) (RTree a) (RTree a)
                    deriving (Show, Eq, Ord)

{- (a) foldTree - 8% -}
foldTree op (LEAF a) = a
-- do op operation with and the result of op from 2 recursive function to t1 and t2
foldTree op (NODE a t1 t2) = op a (op (foldTree op t1)(foldTree op t2))
{- (b) createRTree - 12% -}
createRTree (LEAF a) = RLEAF a
createRTree (NODE a t1 t2) = RNODE a (foldTree min (NODE a t1 t2), (foldTree max (NODE a t1 t2))) (createRTree t1) (createRTree t2)

--tree1=NODE 5(NODE 1(NODE 2(LEAF 4)(LEAF 5))(LEAF 6))(NODE 10(LEAF 8)(LEAF 9))
--tree2=NODE "F"(NODE "D"(LEAF "E")(NODE "C"(LEAF "B")(LEAF "G")))(NODE "G"(NODE "H"(LEAF "F")(LEAF "E"))(LEAF "A"))
{-tree3 = NODE 1 (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6)) 
               (NODE 7 (LEAF 8) (LEAF 9))

tree4 = NODE 1 
          (NODE 3 
               (NODE 7 
                    (LEAF 15) 
                    (LEAF 17))  
               (LEAF 9))   
          (NODE 5 
               (NODE 11 
                    (LEAF 19) 
                    (LEAF 21)) 
               (LEAF 13))
         -}      
{- (c) fastSearch - 15% -}
fastSearch rtree x = reverse(fastHelper rtree x [])
                    where
                         fastHelper (RLEAF a) x buf = [("leaf", a)]
                                                                           -- if in range keep, keep going down the tree
                         fastHelper (RNODE a (min, max) lnode rnode) x buf | (x <= max && x >= min) = (fastHelper rnode x buf) ++ (fastHelper lnode x buf) ++ [("node", a)] ++ buf
                                                                           -- stop searching when x is not in range, add node to buf
                                                                           | otherwise = ("node", a) : buf
--rtree1=RNODE 5(1,10)(RNODE 1(1,6)(RNODE 2(2,5)(RLEAF 4)(RLEAF 5))(RLEAF 6))(RNODE 10(8,10)(RLEAF 8)(RLEAF 9))
--rtree2=RNODE"F"("A","H")(RNODE"D"("B","G")(RLEAF"E")(RNODE"C"("B","G")(RLEAF"B")(RLEAF"G")))(RNODE"G"("A","H")(RNODE"H"("E","H")(RLEAF"F")(RLEAF"E"))(RLEAF"A"))
-------------------------------------------------------------------

{- Tree Examples 5% -}
-- include your tree examples in the test file. 

{-Testing your tree functions - 5%-}


