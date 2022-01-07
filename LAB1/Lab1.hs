-- CptS 355 - Lab 1 (Haskell) - Fall 2021
-- Name: Khoa Bui
-- WSU ID: 11685409
-- Collaborated with: 

module Lab1
     where


-- 1.insert 
insert n item [] = []
insert 1 item [x] = x:[item]
insert 0 item ls = item:ls
insert n item (x:xs) = x:(insert (n-1) item xs)


-- 2. insertEvery
insertEvery n item ls = insertHelper n item ls n
                        where
                             insertHelper n item [] org_n = []
                             --insertHelper 0 item [] org_n = []
                             insertHelper 1 item [x] org_n = x:[item]
                             insertHelper 0 item ls org_n = item:(insertHelper org_n item ls org_n)
                             insertHelper n item (x:xs) org_n = x:(insertHelper (n-1) item xs org_n)

-- 3. getSales
getSales "" ls = 0
getSales day [] = 0
getSales day ((x,y):xs) | x == day = y + (getSales day xs)
                        | otherwise = getSales day xs

--storelog=[("Mon",50),("Fri",20),("Tue",20),("Fri",10),("Wed",25),("Fri",30)]                    
-- 4. sumSales
sumSales "" "" ls = 0
sumSales "" day ls = 0
sumSales store day [] = 0
sumSales store day ((x,y):xs) | x == store = (getSales day y) + (sumSales store day xs)
                              | otherwise = sumSales store day xs
--sales=[("Amazon",[("Mon",30),("Wed",100),("Sat",200)]),("Etsy",[("Mon",50),("Tue",20),("Wed",25),("Fri",30)]),("Ebay",[("Tue",60),("Wed",100),("Thu",30)]),("Etsy",[("Tue",100),("Thu",50),("Sat",20),("Tue",10)])]
-- 5. split
split c ls = splitHelper c ls []
             where 
                  splitHelper c [] buf | buf == [] = []
                                       | otherwise = (reverse buf):[]
                  
                  splitHelper c (x:xs) buf | x == c = (reverse buf):(splitHelper c xs [])
                                           | otherwise = splitHelper c xs (x:buf)

-- 6. nSplit

