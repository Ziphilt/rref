{- rref.hs, by Caleb Stepanian
 - row reduces matrices of arbitrary size, using lists and rationals
 -
 - EXAMPLE USAGE
*Main> let mat3 = [([2,2,9],[7]),([1,0,-3],[8]),([0,1,5],[-2])] :: AugMatrix
*Main> doRrefAug mat3
    1     0     0 |     5
    0     1     0 |     3
    0     0     1 |    -1
-}

{---- IMPORTS ----} --{{{
import Data.Ratio ((%))
import Data.List (findIndex, sortBy, (\\))
import Data.Function (on)
import GHC.Real -- :% -- TODO: why can't I import just this constructor?
--}}}


{---- TYPE SYNONYMS ----} --{{{
type Val = Ratio Integer
type Row = [Val]
type Matrix = [Row]
type AugRow = (Row, Row)
type AugMatrix = [AugRow]
--}}}


{---- TESTING ----} --{{{
-- unique solution
mat1 = [([0,2,9],[7]),([1,0,-3],[8]),([0,1,5],[-2])] :: AugMatrix
-- no solution
mat2 = [([1,-5,4],[-3]),([2,-7,3],[-2]),([-2,1,7],[-1])] :: AugMatrix
-- unique solution, redundant but consistent
mat3 = [([0,2,9],[7]),([1,0,-3],[8]),([0,1,5],[-2]),([1,0,-3],[8])] :: AugMatrix
-- compute the inverse
mat4 = [([0,2,9],[1,0,0]),([1,0,-3],[0,1,0]),([0,1,5],[0,0,1])] :: AugMatrix
-- infinite solutions
mat5 = [([0,2,9],[7]),([1,0,-3],[8])] :: AugMatrix
-- infinite solutions, redundant row
mat6 = [([0,2,9],[7]),([1,0,-3],[8]),([0,2,9],[7])] :: AugMatrix
-- unique solution and rows of zero
mat7 = [([0,2,9],[7]),([0,0,0],[0]),([1,0,-3],[8]),([0,1,5],[-2]),([0,0,0],[0])] :: AugMatrix
-- contradiction
mat8 = [([0,2,9],[7]),([0,0,0],[9]),([1,0,-3],[8]),([0,1,5],[-2])] :: AugMatrix
-- infinite solutions, zero row
mat9 = [([0,2,9],[7]),([1,0,-3],[8]),([0,0,0],[0])] :: AugMatrix
-- already rref'd, x2, x4 arbitrary
mat10 = [([1,0,1,0,5],[2]),([0,1,3,0,6],[4]),([0,0,0,1,7],[8])] :: AugMatrix
--}}}


{---- PRINTING ----} --{{{

-- pads a string with spaces at the beginning
-- to be at least as long as n
padStr :: Int -> String -> String
padStr n s
  | len < n   = replicate (n-len) ' ' ++ s
  | otherwise = s
  where len = length s

-- convert a Val into a String for pretty-printing
-- only shows a denominator if it is not one
showVal :: Val -> String
showVal (x:%1) = show x
showVal (x:%y) = show x ++ "/" ++ show y

-- convert a Row into a String
-- also pads each Val to five characters for pretty printing
showRow :: Row -> String
showRow = unwords . (map ((padStr 5) . showVal))

-- convert a Matrix into a String
showMatrix :: Matrix -> String
showMatrix = unlines . (map showRow)

-- convert an AugRow into a String
showAugRow :: AugRow -> String
showAugRow (lr, rr) = showRow lr ++ " | " ++ showRow rr

-- convert an AugMatrix into a String
showAugMatrix :: AugMatrix -> String
showAugMatrix = unlines . (map showAugRow)
--}}}


{---- CONVENIENCE FUNCTIONS ----} --{{{
-- maps a function on the left and right sides of an augmented row
mapAug :: (Row -> Row) -> AugRow -> AugRow
mapAug f (lr, rr) = (f lr, f rr)

-- eliminates the second Row using the leading coefficient in the first Row
eliminate :: Row -> Row -> Row
eliminate r1 r2 = zipWith (+) (map (*x) r1) r2
  where
    x        = -(r2!!i1)/x1 -- the value to pivot with
    (i1, x1) = firstNonZeroIndex r1 -- find the first nonzero element of r1, or zero otherwise
    firstNonZeroIndex r = case findIndex (/=0) r of
                        Just i  -> (i, r!!i)
                        Nothing -> (0, 1)

-- eliminates the second AugRow using the leading coefficient in the first AugRow
eliminateAug :: AugRow -> AugRow -> AugRow
eliminateAug (lr1, rr1) (lr2, rr2) = (zipWith (+) (map (*x) lr1) lr2, zipWith (+) (map (*x) rr1) rr2)
  where
    x        = -(lr2!!i1)/x1 -- the value to pivot with
    (i1, x1) = firstNonZeroIndex lr1 -- find the first nonzero element of r1, or zero otherwise
    firstNonZeroIndex r = case findIndex (/=0) r of
                        Just i  -> (i, r!!i)
                        Nothing -> (0, 1)

-- scales a Row so that its first nonzero element is one
leadingOne :: Row -> Row
leadingOne r = map (*(1 / firstNonZero r)) r

-- scales an AugRow so that its first nonzero element is one
leadingOneAug :: AugRow -> AugRow
leadingOneAug ar@(lr, rr) = mapAug (map (*(1 / firstNonZero lr))) ar

-- finds the first nonzero element, or uses 1 if all zeros
-- (helper for leadingOne)
firstNonZero :: Row -> Val 
firstNonZero r = case filter (/=0) r of
               (x:xs) -> x
               _      -> 1

-- determines whether the given Matrix is rectangular and non-empty
sane :: Matrix -> Bool
sane []     = False
sane (r:rs) = all (\row -> (length row)==(length r)) rs && length r /= 0

-- determines whether the given AugMatrix is rectangular and non-empty
saneAug :: AugMatrix -> Bool
saneAug []             = False
saneAug ((lr, rr):ars) = all (\(l, r) -> (eqLen l lr) && (eqLen r rr)) ars -- && length r /= 0
  where
    eqLen :: Row -> Row -> Bool
    eqLen r1 r2 = (length r1)==(length r2)

-- wrapper function for reallyRref, which checks for sanity
rref :: Matrix -> Matrix
rref m
  | sane m    = reallyRref [] m
  | otherwise = error "rref: input matrix is not rectangular or is empty"

-- wrapper function for reallyAugRref, which checks for sanity,
-- prunes rows of zeros, and sorts by leading coefficients
rrefAug :: AugMatrix -> AugMatrix
rrefAug am
  | saneAug am = sortAugMatrix . pruneZeros $ reallyRrefAug [] am
  | otherwise  = error "rrefAug: input matrix is not rectangular or is empty"

-- sort the rows of an AugMatrix by the column of the leading coefficients
sortAugMatrix :: AugMatrix -> AugMatrix
sortAugMatrix = sortBy (compare `on` firstNonZeroIndex)
  where
    firstNonZeroIndex (lr,_) = case findIndex (/=0) lr of
                        Just i  -> i
                        -- just choose the length if no nonzero elements,
                        -- so rows of zero go on the bottom
                        Nothing -> length lr

-- prunes rows of all zeros
pruneZeros :: AugMatrix -> AugMatrix
pruneZeros = filter (notAllZeros)
  where notAllZeros (lr, rr) = not $ all (==0) (lr ++ rr)

-- helper function to print what rref does to a Matrix
doRref :: Matrix -> IO ()
doRref = putStr . showMatrix . rref

-- helper function to print what rrefAug does to an AugMatrix
doRrefAug :: AugMatrix -> IO ()
doRrefAug = putStr . showAugMatrix . rrefAug

-- determines if a system has a contradiction
-- there is one if there is a row with all zeros on the left and any nonzero on the right
hasContradiction :: AugMatrix -> Bool
hasContradiction = any (contradiction)
  where contradiction (lr, rr) = all (==0) lr && any (/=0) rr

-- determines if a system has infinite solutions
-- assumes the system is already rref'd, has had rows of zero pruned,
-- and does not have contradictions
-- there are infinite solutions if the number of variables is greater than the number of equations
hasInfiniteSolutions :: AugMatrix -> Bool
hasInfiniteSolutions []           = False -- empty system does not have infinite solutions
hasInfiniteSolutions m@((lr,_):_) = length lr > length m

-- turns a system into a description of what each variable equals
-- assumes the input system is consistent
showEquations :: AugMatrix -> String
showEquations m = unlines (map (showEq) m ++ findArb m)
  where
    showArb n            = "x" ++ show n ++ " arbitrary"
    findArb m@((lr,_):_) = map (showArb) ([0..(length lr - 1)] \\ map (leadingCoef) m)
      where
        leadingCoef (l,_) = case findIndex (/=0) l of
                             Just i  -> i
                             Nothing -> length l
    showEq (lr, (r1:_))  = "x" ++ show leadingVar ++ " = " ++ showVal r1 ++ (concat $ map (showVar lr) indices)
      where
        indices     = [(leadingVar+1)..(length lr)-1]
        showVar r n = if (r!!n == 0) then "" else " + " ++ (("("++) . (++")") . showVal) (-(r!!n)) ++ "*x"++ show n
        leadingVar  = case findIndex (/=0) lr of
                      Just i  -> i
                      Nothing -> error "showEquations: there isn't supposed to be a zero row here"
{-
x0 =    3*x2 +   8
x1 = -9/2*x2 + 7/2
x2 arbitrary

|x0|      |   3|   |  8|
|x1| = x2*|-9/2| + |7/2|
|x2|      |   1|   |  0|
-}
interpretSystem :: AugMatrix -> String
interpretSystem m = (showAugMatrix reduced) ++ "\n" ++ 
                    if (hasContradiction reduced) then "no solution!\n"
                      else (if (hasInfiniteSolutions reduced) then "infinite solutions:\n"
                        else "unique solution:\n")
                        ++ showEquations reduced
  where
    reduced = rrefAug m
--}}}


{---- DO THE REAL WORK ----} --{{{
-- actually perform row reduction on a Matrix.
-- the first argument is the list of completed rows, which accumulates
-- as it calls itself recursively
reallyRref :: Matrix -> Matrix -> Matrix
reallyRref done []     = done
reallyRref done (r:rs) = reallyRref (elim done ++ [leadingOne r]) (elim rs)
  where
    elim = map (eliminate (leadingOne r))

-- actually perform row reduction on an AugMatrix.
-- the first argument is the list of completed rows, which accumulates
-- as it calls itself recursively
reallyRrefAug :: AugMatrix -> AugMatrix -> AugMatrix
reallyRrefAug done []       = done
reallyRrefAug done (ar:ars) = reallyRrefAug (elim done ++ [leadingOneAug ar]) (elim ars)
  where
    elim = map (eliminateAug (leadingOneAug ar))
--}}}
