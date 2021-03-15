module CALC where


import Data.Char
import Data.List.Split
import Debug.Trace





findDiv :: [String] -> Bool

findDiv []  = False

findDiv list  = if head list == "/" then True else findDiv (tail list)

findMul []  = False

findMul list  = if head list == "*" then True else findMul (tail list)

findPlus [] = False

findPlus list = if head list == "+" then True else findPlus (tail list)

findSub []  = False

findSub list  = if head list == "-" then True else findSub (tail list)

findBr [] = False

findBr list  = if head list == "(" then True else findBr (tail list)


rInt :: String -> Double

rInt = read


calcDiv :: [String] -> [String]

calcDiv  list = if findDiv list == True
				    then if head (tail list) == "/"
				    	 then calcDiv ((show $ rInt (head list) / rInt (head $ tail $ tail list)) : (tail $ tail $ tail list))
				    	 else (head list) : (head (tail list)) : (calcDiv (tail (tail list)))
				 	else list

calcMul  list = if findMul list == True
				    then if head (tail list) == "*"
				    	 then calcMul ((show $ rInt (head list) * rInt (head $ tail $ tail list)) : (tail $ tail $ tail list))
				    	 else (head list) : (head (tail list)) : (calcMul (tail (tail list)))
				 	else list

calcPlus list = if findPlus list == True
				    then if head (tail list) == "+"
				    	 then calcPlus ((show $ rInt (head list) + rInt (head $ tail $ tail list)) : (tail $ tail $ tail list))
				    	 else (head list) : (head (tail list)) : (calcPlus (tail (tail list)))
				 	else list

calcSub  list = if findSub list == True
				    then if head (tail list) == "-"
				    	 then calcSub ((show $ rInt (head list) - rInt (head $ tail $ tail list)) : (tail $ tail $ tail list))
				    	 else (head list) : (head (tail list)) : (calcSub (tail (tail list)))
				 	else list


calculate :: [String] -> [String]

calculate list = calcSub (calcPlus(calcMul(calcDiv list))) 



charToString :: Char -> String 

charToString c = [c]


divString :: String -> [String]

divString [] = []

divString (x:xs) = union [charToString x] (divString xs)



finalList :: [String] -> [String]

finalList [] = []

finalList[a, b] = if a /= "+" &&
					 a /= "-" &&
					 a /= "/" &&
				     a /= "*" &&
				     a /= ")" && 
					 a /= "(" && 
					 b /= "+" &&
					 b /= "-" &&
					 b /= "/" && 
					 b /= "*" &&
					 b /= ")" && 
					 b /= "("   then [a++b] else [a, b]  

finalList (x1:x2:xs) = if x1 /= "+" &&
						  x1 /= "-" &&
						  x1 /= "/" &&
						  x1 /= "*" &&
						  x1 /= ")" && 
					 	  x1 /= "(" && 
						  x2 /= "+" &&
						  x2 /= "-" &&
						  x2 /= "/" && 
						  x2 /= "*" &&
						  x2 /= ")" && 
					 	  x2 /= "("   then finalList ((x1 ++ x2) : xs) else (x1 : finalList (x2 : xs)) 


countBr :: [String] -> Int -> [Int] -> [Int]

countBr [] num list_br = list_br

countBr (x:xs) num list_br | x == "(" = countBr xs (num + 1) (num : list_br)
						   | x == ")" = countBr xs (num + 1) ((-num) : list_br)
						   | otherwise = countBr xs (num + 1) list_br


startList :: String -> [String]

startList string = finalList (divString string)


brInfo :: [String] -> [Int]

brInfo list = reverse (countBr list 0 [])


slice :: Int -> Int -> [a] -> [a]

slice from to xs = take (to - from + 1) (drop from xs)


toString :: [String] -> String

toString [] = []

toString (x:xs) = x ++ toString xs


union :: [String] -> [String] -> [String]

union [] list = list

union list1 list2 = union (init list1) (last list1 : list2)


calc :: [String] -> [Int] -> Int -> [String]

calc list brlist n = if n >=0 then if (brlist !! n) >= 0 && (brlist !! (n + 1)) <= 0 
					 			   then calc newlist (brInfo newlist) (n - 1)
					          	   else calc list brlist (n + 1)
					 		  else list
					 			   where newlist = union (union (take (brlist !! n) list) (calculate (slice ((brlist !! n) + 1) ((abs (brlist !! (n+1))) - 1) list))) (drop (abs (brlist !! (n+1)) + 1) list) 


calcul :: [String] -> [String]

calcul list = calc list (brInfo list) 0


killBr :: [String] -> [String]

killBr list = if findBr list == True then killBr (calcul list) else list 


finalCalc :: String -> [String]

finalCalc string = calculate (killBr (startList string))




{-
splitOpBr string = split (condense $ oneOf ")+-*/(") string



correction [] = []

correction (x:xs) | x == "" = correction xs
				  | x == "*(" = "*" : "(" : correction xs
				  | x == "+(" = "+" : "(" : correction xs
				  | x == "-(" = "-" : "(" : correction xs
				  | x == "/(" = "/" : "(" : correction xs
				  | x == ")*" = ")" : "*" : correction xs
				  | x == ")+" = ")" : "+" : correction xs
				  | x == ")-" = ")" : "-" : correction xs
				  | x == ")/" = ")" : "/" : correction xs
				  | x == ")*(" = ")" : "*" : "(" : correction xs
				  | x == ")+(" = ")" : "+" : "(" : correction xs
				  | x == ")-(" = ")" : "-" : "(" : correction xs
				  | x == ")/(" = ")" : "/" : "(" : correction xs
				  | x == "+(("  = "+" : "(" : "(" : correction xs
				  | x == "))+"  = ")" : ")" : "+" : correction xs
				  | x == "*(("  = "*" : "(" : "(" : correction xs
				  | x == "))*"  = ")" : ")" : "*" : correction xs
				  | x == "/(("  = "/" : "(" : "(" : correction xs
				  | x == "))/"  = ")" : ")" : "/" : correction xs
				  | x == "-(("  = "-" : "(" : "(" : correction xs
				  | x == "))-"  = ")" : ")" : "-" : correction xs
				  | x == "))" = ")" : ")" : correction xs
				  | x == "((" = "(" : "(" : correction xs
				  | otherwise = x : correction xs
-}





{-
calc list brlist n = if n >=0 then if (brlist !! n) > 0 && (brlist !! (n + 1)) < 0 
					 			   then (calc ((take (brlist !! n) list) : (calculate (slice ((brlist !! n) + 1) ((abs (brlist !! (n+1))) - 1) list)) : (drop (length list - (abs (brlist !! (n+1))) - 1) list)) ((take n brlist) : (drop ((length brlist) - n - 1) brlist)) (n - 1))
					          else calc list brlist (n+1)
					 else list

help list brlist n = (take (brlist !! n) list) : (calculate (toString (slice ((brlist !! n) + 1) ((abs (brlist !! (n+1))) - 1) list))) : (drop (length list - (abs (brlist !! (n+1))) - 1) list)

help1 list brlist n = (take (brlist !! n) list) : (drop (length list - (abs (brlist !! (n+1))) - 1) list)

help list brlist n = union (union (take (brlist !! n) list) (calculate (toString (slice ((brlist !! n) + 1) ((abs (brlist !! (n+1))) - 1) list)))) (drop (length list - (abs (brlist !! (n+1))) - 1) list)

help list brlist n = drop (abs (brlist !! (n+1)) + 1) list

calcH string = help (startList string) (brInfo string) 0
-}



{-
cti x = ord x - 48
itc x = chr (x + 48)

findMul [] = False
findMul (x:xs) = if x == '*' then True else findMul xs 

second a = head (tail a)

calcMul (x:xs) = if findMul (x:xs) == True
				    then if head xs == '*'
				    	 then calcMul (itc (cti x * cti (second xs)) : tail (tail xs))
				    	 else x : head xs : (calcMul (tail xs)) 
				 else (x:xs)

culcMul (x:op:xs) = if findMul (x:op:xs) == True
				    then if op == '*'
				    	 then culcMul (itc (cti x * cti (head xs)) : tail xs)
				    	 else x : op : (culcMul xs) 
				    else (x:op:xs)
-}