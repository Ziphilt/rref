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

import Data.Ratio ((%))
import Data.List (findIndex, sortBy)
import Data.Function (on)
import GHC.Real -- :% -- TODO: why can't I import just this constructor?

{---- TYPE SYNONYMS ----} --{{{
type Val = Ratio Integer
type Row = [Val]
type Matrix = [Row]
type AugRow = (Row, Row)
type AugMatrix = [AugRow]
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

-- wrapper function for reallyAugRref, which checks for sanity
rrefAug :: AugMatrix -> AugMatrix
rrefAug am
  | saneAug am = sortAugMatrix $ reallyRrefAug [] am
  | otherwise  = error "rrefAug: input matrix is not rectangular or is empty"

-- helper function to print what rref does to a Matrix
doRref :: Matrix -> IO ()
doRref = putStr . showMatrix . rref

-- helper function to print what rrefAug does to an AugMatrix
doRrefAug :: AugMatrix -> IO ()
doRrefAug = putStr . showAugMatrix . rrefAug

-- sort the rows of an AugMatrix by the column of the leading coefficients
sortAugMatrix :: AugMatrix -> AugMatrix
sortAugMatrix = sortBy (compare `on` firstNonZeroIndex)
  where
    firstNonZeroIndex (lr, _) = case findIndex (/=0) lr of
                        Just i  -> i
                        -- just choose the length if no nonzero elements,
                        -- so rows of zero go on the bottom
                        Nothing -> length lr
--}}}


{---- DO THE REAL WORK ----} --{{{

-- actually perform row reduction on a Matrix.
-- the first argument is the list of completed rows, which accumulates
-- as it calls itself recursively
reallyRref :: Matrix -> Matrix -> Matrix
reallyRref done []     = done
reallyRref done (r:rs) = reallyRref (elim done ++ [leadingOne r]) (elim rs)
  where
    elim          = map (eliminate (leadingOne r))

-- actually perform row reduction on an AugMatrix.
-- the first argument is the list of completed rows, which accumulates
-- as it calls itself recursively
reallyRrefAug :: AugMatrix -> AugMatrix -> AugMatrix
reallyRrefAug done []       = done
reallyRrefAug done (ar:ars) = reallyRrefAug (elim done ++ [leadingOneAug ar]) (elim ars)
  where
    elim          = map (eliminateAug (leadingOneAug ar))
--}}}
