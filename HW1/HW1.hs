-- CptS 355 - Fall 2021 -- Homework1 - Haskell
-- Name: Khoa Bui - 11685409
-- Collaborators: 

module HW1
     where

-- Q1 everyOther
everyOther [] = []
everyOther [x] = [x]
-- x is every other value and will be included in the list
everyOther (x:y:xs) = x:everyOther(xs) 

-- Q2(a) eliminateDuplicates
eliminateDuplicates [] = []
eliminateDuplicates (x:xs) | (x `elem` xs) = eliminateDuplicates xs -- check if x is in the rest of the list
                           | otherwise = x:(eliminateDuplicates xs)

-- Q2(b) matchingSeconds
matchingSeconds v [] = []
matchingSeconds v ((x,y):xs) | (x == v) = y:(matchingSeconds v xs) -- check if the first value in the pair matches v
                             | otherwise = matchingSeconds v xs
-- Q2(c) clusterCommon
clusterCommon ls = clusterHelper ls ls
                   where
                   clusterHelper [] _= []
                   -- add up all second values that has the same first value
                   -- recursively iterate through every pair
                   -- eliminate the duplicates
                   clusterHelper ((x,y):xs) ls = eliminateDuplicates((x,(matchingSeconds x ls)) : (clusterHelper xs ls))

-- Q3 maxNumCases

maxNumCases ls month = maxHelper ls month 0
                       where
                       maxHelper [] month max = max
                       --maxHelper [] month 0 = 0
                       maxHelper ls "" 0 = 0
                       maxHelper ((c,((a,b):ys)):xs) month max | (max >= b && a == month) = maxHelper xs month max
                                                               | (a /= month && ys /= []) = maxHelper ((c,ys):xs) month max
                                                               | (ys == []) = maxHelper xs month max
                                                               | otherwise = maxHelper xs month b

-- Q4 groupIntoLists
groupIntoLists ls = groupHelper ls 1 1 []
                    where
                    -- when the list is empty if leftover ls will be a separated list     
                    groupHelper [] m n ls | ls == [] = [] 
                                          | otherwise = (reverse ls) : []
                    -- decrement n from m to 1
                    -- add x to ls if n >= 1, otherwise add reversed ls to the recursive call while increment m and create the next sublist
                    groupHelper (x:xs) m n ls | (n >= 1) = groupHelper xs m (n-1) (x:ls)
                                              | otherwise = (reverse ls) : (groupHelper (x:xs) (m+1) (m+1) [])


-- Q5 getSlice 
getSlice de lb = sliceHelper de lb []
               where                      -- list is empty and buf is empty (could not find delimiter), return []
                    sliceHelper (a,b) [] buf = []
                                                  -- gets a and buf is empty, starts adding char to buf
                    sliceHelper (a,b) (x:y:xs) buf | (x == a && buf == []) = sliceHelper (a,b) xs (y:buf)
                                                  -- gets b and buf is not empty, return reverse buf
                                                   | (x == b) = reverse buf
                                                   -- buf is not empty, either x /= b, x = a, x /= a
                                                   -- moves to next char and add x to buf
                                                   | (buf /= []) = sliceHelper (a,b) (y:xs) (x:buf)
                                                   -- buf is empty, either x /= a, x /= b, x = b
                                                   -- moves to next char but not adding x to buf
                                                   | otherwise = sliceHelper (a,b) (y:xs) buf
                    sliceHelper (a,b) [x] buf | buf == [] = []
                                              | (x == a) = reverse buf
                                              | otherwise = reverse (x:buf)

