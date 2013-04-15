{- rref.hs, by Caleb Stepanian
 - row reduces matrices of arbitrary size, using lists and rationals
 -
 - EXAMPLE USAGE
 > let matrix = [[0,1,5,-4],[1,4,3,-2],[2,7,1,-2]] :: Matrix
 > doRref matrix
 0 1 5 0
 1 0 -17 0
 0 0 0 -2
  *Main> let matA = [[3,4,1],[-4,3,0],[2,7,-6%7]] :: Matrix
  *Main> let matB = [[91],[-2%9],[5]] :: Matrix
  *Main> putStr $ showAugMatrix (matA,matB)
      3     4     1 |    91
     -4     3     0 |  -2/9
      2     7  -6/7 |     5
  *Main> putStr $ showMatrix matA
      3     4     1
     -4     3     0
      2     7  -6/7
  *Main> putStr $ showMatrix matB
     91
   -2/9
      5
 -}

import Data.Ratio ((%))
import Data.List (findIndex)
import GHC.Real -- :%
type Val = Ratio Integer
type Row = [Val]
type Matrix = [Row]
type AugMatrix = (Matrix, Matrix)

-- eliminates the second Row using the leading coefficient in the first Row
eliminate :: Row -> Row -> Row
eliminate r1 r2 = zipWith (+) (map (*x) r1) r2
  where
    x = -(r2!!i1)/x1 -- the value to pivot with
    (i1, x1) = firstNonZeroIndex r1 -- find the first nonzero element of r1, or zero otherwise
    firstNonZeroIndex r = case findIndex (/=0) r of
                        Just i  -> (i, r!!i)
                        Nothing -> (0, 1)

-- scales a Row so that its first nonzero element is one
leadingOne :: Row -> Row
leadingOne r = map (*(1 / firstNonZero r)) r

-- finds the first nonzero element, or uses 1 if all zeros
-- (helper for leadingOne)
firstNonZero :: Row -> Val 
firstNonZero r = case filter (/=0) r of
               (x:xs) -> x
               _      -> 1

-- convert a Val into a String for pretty-printing
showVal :: Val -> String
showVal (x:%1) = show x
showVal (x:%y) = show x ++ "/" ++ show y

-- pads a string with spaces at the beginning
-- to be at least as long as n
padStr :: Int -> String -> String
padStr n s
  | len < n   = replicate (n-len) ' ' ++ s
  | otherwise = s
  where len = length s

-- convert a Row into a String
showRow :: Row -> String
showRow = unwords . (map ((padStr 5) . showVal))

-- convert a Matrix into a String
showMatrix :: Matrix -> String
showMatrix = unlines . (map showRow)

-- convert an AugMatrix into a String
showAugMatrix :: AugMatrix -> String
showAugMatrix ([],[]) = []
showAugMatrix (lrow:lrows, rrow:rrows) = showRow lrow ++ " | " ++ showRow rrow ++ "\n" ++
                                         showAugMatrix (lrows,rrows)

-- determines whether the given Matrix is rectangular and non-empty
sane :: Matrix -> Bool
sane (r:rs) = all (\row -> (length row)==(length r)) rs && length r /= 0

-- wrapper function for reallyRref, which checks for sanity
rref :: Matrix -> Matrix
rref m
  | sane m    = reallyRref [] m
  | otherwise = error "rref: input matrix is not rectangular or is empty"

-- actually perform row reduction on a Matrix.
-- the first argument is the list of completed rows, which accumulates
-- as it calls itself recursively
reallyRref :: Matrix -> Matrix -> Matrix
reallyRref done []   = done
reallyRref done (r:rs) = reallyRref (elim done ++ [leadingOne r]) (elim rs)
  where
    elim          = map (eliminate (leadingOne r))
    --((r:nzr), zr) = partition (\row -> (head row)/=0) rest
    --rs            = nzr ++ zr

-- helper function to print what rref does to a matrix
doRref :: Matrix -> IO ()
doRref = putStr . showMatrix . rref
