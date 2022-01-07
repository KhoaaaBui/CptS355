{-Haskell HW2 HUnit test cases
 Please add at least 2 additional tests for problems 2(a,b) and 4(a,b,c)-}

module HW2SampleTests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
import HW2

-- Sample Tree examples given in the assignment prompt; make sure to provide your own tree examples
-- Your trees should have minimum 4 levels. 
tree1 = NODE 5 (NODE 1 (NODE 2 (LEAF 4) (LEAF 5)) (LEAF 6)) 
               (NODE 10 (LEAF 8) (LEAF 9))

tree2 = NODE "F" (NODE "D" (LEAF "E") (NODE "C" (LEAF "B") (LEAF "G")))
                           (NODE "G" (NODE "H" (LEAF "F") (LEAF "E")) (LEAF "A")) 


tree3 = NODE 1 (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6)) 
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

rtree1 =  RNODE 5 (1,10) (RNODE 1 (1,6) (RNODE 2 (2,5) (RLEAF 4) (RLEAF 5)) (RLEAF 6)) (RNODE 10 (8,10) (RLEAF 8) (RLEAF 9))

rtree2 =  RNODE "F" ("A","H") (RNODE "D" ("B","G") (RLEAF "E") (RNODE "C" ("B","G") (RLEAF "B") (RLEAF "G"))) 
                              (RNODE "G" ("A","H") (RNODE "H" ("E","H") (RLEAF "F") (RLEAF "E")) (RLEAF "A"))

rtree3 = RNODE 1 (1,9) (RNODE 2 (2,6) (RNODE 3 (3,5) (RLEAF 4) (RLEAF 5)) (RLEAF 6)) (RNODE 7 (7,9) (RLEAF 8) (RLEAF 9))
--rtree3 = RNODE 1 (1,9) (RNODE 2 (2,6) (RNODE 3 (3,5) (RLEAF 4) (RLEAF 5)) (RLEAF 6)) (RNODE 7 (7,9) (RLEAF 8) (RLEAF 9))

--rtree4 = RNODE 1 (1,21) (RNODE 3 (3,17) (RNODE 7 (7,17) (RLEAF 15) (RLEAF 17)) (RLEAF 9)) (RNODE 5 (5,21) (RNODE 11 (11,21) (RLEAF 19) (RLEAF 21)) (RLEAF 13))                 
rtree4 = RNODE 1 (1,21) (RNODE 3 (3,17) (RNODE 7 (7,17) (RLEAF 15) (RLEAF 17)) (RLEAF 9)) (RNODE 5 (5,21) (RNODE 11 (11,21) (RLEAF 19) (RLEAF 21)) (RLEAF 13))
p1_test1 = TestCase (assertEqual "groupbyNTail-test1" [[1,2,3,4],[5,6,7,8],[9,10,11,12],[13,14,15]] (groupbyNTail [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15] 4) )  
p1_test2 = TestCase (assertEqual "groupbyNTail-test2" ["abcde","fghij","klmno","pqrst","uwxyz","012"]  (groupbyNTail "abcdefghijklmnopqrstuwxyz012" 5) ) 

p2a_test1 = TestCase (assertEqual "elemAll-test1" (True)  (elemAll [3,5,7,10]  [1,2,3,4,5,6,7,8,9,10]) )  
p2a_test2 = TestCase (assertEqual "elemAll-test2" (False) (elemAll [3,5,10]  [1,2,3,4,5,6,7,8,9]) ) 
p2a_test3 = TestCase (assertEqual "elemAll-test3" (True)  (elemAll ["Bishop", "TerreView", "Walmart"] ["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay", "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"]) ) 
p2a_test4 = TestCase (assertEqual "elemAll-test4" (False) (elemAll ["Bishop", "TerreView"] ["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Shopco", "RockeyWay"]) ) 
p2a_test5 = TestCase (assertEqual "elemAll-test5" (False) (elemAll [1,2,3] []))
p2a_test6 = TestCase (assertEqual "elemAll-test6" (False) (elemAll ([]::[Int]) []))


p2b_test1 = TestCase (assertEqual "stopsAt-test1" (sort ["Wheat"]) 
                                                  (sort $ stopsAt ["Bishop", "TerreView", "Walmart"] buses) ) 
p2b_test2 = TestCase (assertEqual "stopsAt-test2" (sort ["Wheat","Silver"]) 
                                                  (sort $ stopsAt ["Bishop", "Walmart"] buses) ) 
p2b_test3 = TestCase (assertEqual "stopsAt-test3" (sort []) 
                                                  (sort $  stopsAt ["TransferStation", "State", "Main"] buses) ) 
p2b_test4 = TestCase (assertEqual "stopsAt-test4" (sort ([]::[[Char]])) 
                                                  (sort $  stopsAt ["TransferStation", "State", "Main"] ([]::[([Char], [[Char]])]) ))
p2b_test5 = TestCase (assertEqual "stopsAt-test5" (sort []) 
                                                  (sort $  stopsAt [] buses) )  
buses= [("Wheat",["Chinook","Orchard","Valley","Maple","Aspen","TerreView","Clay","Dismores","Martin","Bishop","Walmart","PorchLight","Campus"]),("Silver",["TransferStation","PorchLight","Stadium","Bishop","Walmart","Shopco","RockeyWay"]),("Blue",["TransferStation","State","Larry","TerreView","Grand","TacoBell","Chinook","Library"]),("Gray",["TransferStation","Wawawai","Main","Sunnyside","Crestview","CityHall","Stadium","Colorado"])]
p3a_test1 = TestCase (assertEqual "isBigger-test1" (True) (isBigger (DATE (5,20,2021)) (DATE (5,15,2021)) )) 
p3a_test2 = TestCase (assertEqual "isBigger-test2" (True) (isBigger (DATE (6,10,2021)) (DATE (5,15,2021)) ))
p3a_test3 = TestCase (assertEqual "isBigger-test3" (True) (isBigger (DATETIME (6,10,2021,19,30)) (DATETIME (6,10,2021,19,10)) ))
p3a_test4 = TestCase (assertEqual "isBigger-test4" (False) (isBigger (DATETIME (6,9,2021,19,30)) (DATETIME (6,10,2021,11,10)) ))
p3a_test5 = TestCase (assertEqual "isBigger-test5" (False) (isBigger (DATETIME (6,10,2021,11,10)) (DATETIME (6,10,2021,11,10)) ))
p3a_test6 = TestCase (assertEqual "isBigger-test6" (True) (isBigger (DATE (6,10,2021)) (DATETIME (6,9,2021,11,10)) ))
p3a_test7 = TestCase (assertEqual "isBigger-test7" (False) (isBigger (DATE (6,10,2021)) (DATETIME (6,10,2021,11,10)) ))
p3a_test8 = TestCase (assertEqual "isBigger-test8" (False) (isBigger (DATETIME (6,10,2021,11,10)) (DATE (6,10,2021)) ))

datelist = [DATE(5,28,2021), DATETIME(6,1,2021,14,15), DATE(6,22,2021), DATE(6,1,2021), DATETIME(6,21,2021,15,20), 
            DATETIME(5,21,2020,14,40), DATE (5,20,2021), DATETIME (6,9,2021,19,30), DATETIME (6,10,2021,11,10)]

p3b_test1 = TestCase (assertEqual "applyRange-test1" 
                                  ([DATE (5,28,2021),DATETIME (6,1,2021,14,15),DATE (6,1,2021),DATETIME (6,21,2021,15,20),DATETIME (6,9,2021,19,30),DATETIME (6,10,2021,11,10)]) 
                                  (applyRange (DATE(5,20,2021) , DATETIME(6,21,2021,19,00)) datelist ) ) 
p3b_test2 = TestCase (assertEqual "applyRange-test2" 
                                  ([DATE (6,22,2021),DATETIME (6,21,2021,15,20),DATETIME (6,9,2021,19,30),DATETIME (6,10,2021,11,10)]) 
                                  (applyRange (DATETIME(6,1,2021,14,20) , DATE(6,25,2021)) datelist) ) 


p4a_test1 = TestCase (assertEqual "foldTree-test1" 50  (foldTree (+) tree1) ) 
p4a_test2 = TestCase (assertEqual "foldTree-test2" 10  (foldTree max tree1) ) 
p4a_test3 = TestCase (assertEqual "foldTree-test3" 1  (foldTree min tree1) ) 
p4a_test4 = TestCase (assertEqual "foldTree-test4" "H"  (foldTree max tree2) ) 
p4a_test5 = TestCase (assertEqual "foldTree-test5" "A"  (foldTree min tree2) ) 
p4a_test6 = TestCase (assertEqual "foldTree-test6" "FDECBGGHFEA"  (foldTree (++) tree2) ) 
p4a_test7 = TestCase (assertEqual "foldTree-test7" 45  (foldTree (+) tree3) ) 
p4a_test8 = TestCase (assertEqual "foldTree-test8" 1  (foldTree min tree4) )                                         

p4b_test1 = TestCase (assertEqual "createRTree-test1" (rtree1) (createRTree tree1) ) 
p4b_test2 = TestCase (assertEqual "createRTree-test2" (rtree2)  (createRTree tree2) ) 
p4b_test3 = TestCase (assertEqual "createRTree-test3" (rtree3)  (createRTree tree3) ) 
p4b_test4 = TestCase (assertEqual "createRTree-test4" (rtree4)  (createRTree tree4) ) 

p4c_test1 = TestCase (assertEqual "fastSearch-test1" ([("node",5),("node",1),("node",2),("leaf",6),("node",10)]) 
                                                     (fastSearch rtree1 6) ) 
p4c_test2 = TestCase (assertEqual "fastSearch-test2" ([("node",5),("node",1),("node",10),("leaf",8),("leaf",9)])  
                                                     (fastSearch rtree1 8) ) 
p4c_test3 = TestCase (assertEqual "fastSearch-test3" ([("node","F"),("node","D"),("node","G"),("node","H"),("leaf","A")] )
                                                     (fastSearch rtree2 "A") ) 
p4c_test4 = TestCase (assertEqual "fastSearch-test4" ([("node","F"),("node","D"),("leaf","E"),("node","C"),("leaf","B"),("leaf","G"),("node","G"),("node","H"),("leaf","F"),("leaf","E"),("leaf","A")] )
                                                     (fastSearch rtree2 "F") ) 
p4c_test5 = TestCase (assertEqual "fastSearch-test5" ([("node",1),("node",2),("node",7),("leaf",8),("leaf",9)]) 
                                                     (fastSearch rtree3 8) ) 
p4c_test6 = TestCase (assertEqual "fastSearch-test6" ([("node",1),("node",3),("node",5),("node",11),("leaf",19),("leaf",21),("leaf",13)]) 
                                                     (fastSearch rtree4 19) ) 

tests = TestList [ TestLabel "Problem 1 - test1 " p1_test1,
                   TestLabel "Problem 1 - test2 " p1_test2,
                   TestLabel "Problem 2a - test1 " p2a_test1,
                   TestLabel "Problem 2a - test2 " p2a_test2,  
                   TestLabel "Problem 2a - test3 " p2a_test3,
                   TestLabel "Problem 2a - test4 " p2a_test4, 
                   TestLabel "Problem 2a - test5 " p2a_test5, 
                   TestLabel "Problem 2a - test6 " p2a_test6, 
                   TestLabel "Problem 2b - test1 " p2b_test1,
                   TestLabel "Problem 2b - test2 " p2b_test2,  
                   TestLabel "Problem 2b - test3 " p2b_test3, 
                   TestLabel "Problem 2b - test4 " p2b_test4,
                   TestLabel "Problem 2b - test5 " p2b_test5, 
                   TestLabel "Problem 3a - test1 " p3a_test1,
                   TestLabel "Problem 3a - test2 " p3a_test2,  
                   TestLabel "Problem 3a - test3 " p3a_test3, 
                   TestLabel "Problem 3a - test4 " p3a_test4, 
                   TestLabel "Problem 3a - test5 " p3a_test5, 
                   TestLabel "Problem 3a - test6 " p3a_test6, 
                   TestLabel "Problem 3a - test7 " p3a_test7, 
                   TestLabel "Problem 3a - test8 " p3a_test8, 
                   TestLabel "Problem 3b - test1 " p3b_test1,
                   TestLabel "Problem 3b - test2 " p3b_test2,
                   TestLabel "Problem 4a - test1 " p4a_test1,
                   TestLabel "Problem 4a - test2 " p4a_test2,
                   TestLabel "Problem 4a - test3 " p4a_test3,
                   TestLabel "Problem 4a - test4 " p4a_test4,
                   TestLabel "Problem 4a - test5 " p4a_test5,
                   TestLabel "Problem 4a - test6 " p4a_test6,
                   TestLabel "Problem 4a - test7 " p4a_test7,
                   TestLabel "Problem 4a - test8 " p4a_test8,
                   TestLabel "Problem 4b - test1 " p4b_test1,
                   TestLabel "Problem 4b - test2 " p4b_test2,
                   TestLabel "Problem 4b - test3 " p4b_test3,
                   TestLabel "Problem 4b - test4 " p4b_test4,
                   TestLabel "Problem 4c - test1 " p4c_test1,
                   TestLabel "Problem 4c - test2 " p4c_test2,
                   TestLabel "Problem 4c - test3 " p4c_test3,
                   TestLabel "Problem 4c - test4 " p4c_test4,
                   TestLabel "Problem 4c - test5 " p4c_test5,
                   TestLabel "Problem 4c - test6 " p4c_test6                   
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests