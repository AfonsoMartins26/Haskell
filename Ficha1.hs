module PF where

perimetro :: Float -> Float
perimetro r = 2 * 3.14 * r 


dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt ((x2-x1)^2) + ((y2-y1)^2)

primUlt :: [a] -> (a,a)
primUlt l = (head l, last l)


multiplo :: Int -> Int -> Bool
multiplo m n = if mod m n == 0 then True else False 
                

truncaImpar :: [a] -> [a]
truncaImpar l = if mod(length l)2 == 1
            then tail l
            else l

nRaizes ::(Float,Float,Float) -> Int
nRaizes (a,b,c) = if d <0 then 0
                  else if d > 0 then 2
                  else 1
                  where d = b^2 -4*a*c



raizes :: (Float,Float,Float) -> [Float]
raizes (a,b,c) = if n== 0 then []
                  else if n == 1 then [(-b)/(2*a)]
                  else [(-b* sqrt d)*(2*a), (-b -sqrt d)/(2*a)]
                  where n = nRaizes (a,b,c)
                        d = b^2 -4*a*c