-- CptS 355 - Lab 2 (Haskell) - Fall 2021
-- Name: Khoa Bui
-- Collaborated with: 

module Lab2
     where


-- 1
{- (a) merge2 -}
merge2 [] l2 = l2
merge2 l1 [] = l1
merge2 (x:xs) (y:ys) = x : y : (merge2 xs ys)
                         

{- (b) merge2Tail -}
merge2Tail l1 l2 = merge2TailHelper l1 l2 []
                    where
                         merge2TailHelper [] l2 l3 = reverse(l3) ++ l2
                         merge2TailHelper l1 [] l3 = reverse(l3) ++ l1
                         merge2TailHelper (x:xs) (y:ys) l3 = merge2TailHelper xs ys (y:x:l3) 


{- (c) mergeN -}
mergeN [] = []
mergeN (x:xs) = foldl merge2 x xs



-- 2
{- (a) count -}
count v [] = 0
count v ls = length(filter (==v) ls)


{- (b) histogram  -}
histogramHelper [] = []
histogramHelper (x:xs) | x `elem` xs = histogramHelper xs
                       | otherwise = x : (histogramHelper xs)
-- use count in 2a
histogram [] = []
histogram ls = histogramHelper (map (\x -> (x, count x ls)) ls)


-- 3                
{- (a) concatAll -}
concatAll [] = []
concatAll ls = concatHelper (map concatHelper ls)
               where
                    concatHelper ls = foldr (++) "" ls



{- (b) concat2Either -}               
data AnEither  = AString String | AnInt Int
                deriving (Show, Read, Eq)
--concat2Either [] = AString ""
concat2Either ls = concat2EitherHelper (map concat2EitherHelper ls)
               where
                    concat2EitherHelper [] = AString ""
                    concat2EitherHelper ls = foldr (concat2E) (AString "") ls
                    concat2E (AString a) (AString b)  = AString (a++b)
                    concat2E (AString a) (AnInt b) = AString (a ++ show(b))
                    concat2E (AnInt a) (AString b) = AString (show(a) ++ b)
                    concat2E (AnInt a) (AnInt b) = AString (show(a) ++ show(b))



-- 4      
{-  concat2Str -}               
concat2Str ls = foldr (++) "" (map concat2S ls)
                where   
                     concat2S [] = ""
                     concat2S ls = foldr (concat2Sx) "" ls
                     concat2Sx (AString a) b = a ++ b
                     concat2Sx (AnInt a) b = show(a) ++ b


data Op = Add | Sub | Mul | Pow
          deriving (Show, Read, Eq)

evaluate:: Op -> Int -> Int -> Int
evaluate Add x y =  x+y
evaluate Sub   x y =  x-y
evaluate Mul x y =  x*y
evaluate Pow x y = x^y

data ExprTree a = ELEAF a | ENODE Op (ExprTree a) (ExprTree a)
                  deriving (Show, Read, Eq)

-- 5 
{- evaluateTree -}



-- 6
{- printInfix -}



--7
{- createRTree -}
data ResultTree a  = RLEAF a | RNODE a (ResultTree a) (ResultTree a)
                     deriving (Show, Read, Eq)






