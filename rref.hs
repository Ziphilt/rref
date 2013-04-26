-- rref.hs, by Caleb Stepanian
-- row reduces matrices of arbitrary size, using lists and rationals
-- MA 2071 D Term Project
-- April 2013


{---- IMPORTS ----} --{{{
import System.Console.Readline             -- enables convenient line-editing for input
import Text.ParserCombinators.Parsec       -- parsing library
import Data.Ratio ((%))                    -- rational number library
import Data.List (findIndex, sortBy, (\\))
import Data.Function (on)
import GHC.Real -- :%
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
-- only shows the denominator if it is not equal to 1
showVal :: Val -> String
showVal (x:%1) = show x
showVal (x:%y) = show x ++ "/" ++ show y

-- convert a Row into a String
-- also pads each Val to five characters for pretty printing
showRow :: Row -> String
showRow = unwords . (map ((padStr 5) . showVal))

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

-- eliminates the second AugRow using the leading coefficient in the first AugRow
eliminateAug :: AugRow -> AugRow -> AugRow
eliminateAug (lr1, rr1) (lr2, rr2) = (zipWith (+) (map (*x) lr1) lr2, zipWith (+) (map (*x) rr1) rr2)
  where
    x        = -(lr2!!i1)/x1 -- the value to pivot with
    (i1, x1) = firstNonZeroIndex lr1 -- find the first nonzero element of r1, or zero otherwise
    firstNonZeroIndex r = case findIndex (/=0) r of
                        Just i  -> (i, r!!i)
                        Nothing -> (0, 1)

-- scales an AugRow so that its first nonzero element is one
leadingOneAug :: AugRow -> AugRow
leadingOneAug ar@(lr, rr) = mapAug (map (*(1 / firstNonZero lr))) ar

-- finds the first nonzero element, or uses 1 if all zeros
-- (helper for leadingOneAug)
firstNonZero :: Row -> Val 
firstNonZero r = case filter (/=0) r of
               (x:xs) -> x
               _      -> 1

-- determines whether the given AugMatrix is rectangular and non-empty
saneAug :: AugMatrix -> Bool
saneAug []             = False
saneAug ((lr, rr):ars) = all (\(l, r) -> (eqLen l lr) && (eqLen r rr)) ars
  where
    eqLen :: Row -> Row -> Bool
    eqLen r1 r2 = (length r1)==(length r2)

-- wrapper function for reallyAugRref, which checks for sanity,
-- prunes rows of zeros, and sorts by leading coefficients
rrefAug :: AugMatrix -> AugMatrix
rrefAug am
  | saneAug am = sortAugMatrix . pruneZeros $ reallyRrefAug [] am
  | otherwise  = error "rrefAug: input matrix is not rectangular"

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

-- determines if a system has a contradiction
-- there is one if there is a row with all zeros on the left and any nonzero on the right
-- assumes the system is already rref'd
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
-- assumes the input system is consistent,
-- but does work on both infinite solutions and unique solutions
showEquations :: AugMatrix -> String
showEquations m = unlines (map (showEq) m ++ findArb m)
  where
    showArb n            = "x" ++ show n ++ " arbitrary"
    findArb m@((lr,_):_) = map (showArb) ([0..(length lr - 1)] \\ map (leadingCoef) m)
      where
        leadingCoef (l,_) = case findIndex (/=0) l of
                             Just i  -> i
                             Nothing -> length l

-- shows a row from a system of equations as a relationship between variables
-- helper function for showEquations
showEq :: AugRow -> String
showEq (lr, (r1:_)) = "x" ++ show leadingVar ++ " = " ++ showVal r1 ++ (concat $ map (showVar lr) indices)
  where
    indices     = [(leadingVar+1)..(length lr)-1]
    showVar r n = if (r!!n == 0) then "" else " + " ++ (("("++).(++")") . showVal) (-(r!!n)) ++ "*x" ++ show n
    leadingVar  = case findIndex (/=0) lr of
                    Just i  -> i
                    Nothing -> error "showEq: there isn't supposed to be a zero row here"

-- bundles all the interpretation functions together,
-- and produces the output of the program once the user inputs a matrix
interpretSystem :: AugMatrix -> String
interpretSystem m = (showAugMatrix reduced) ++ "\n" ++ 
                    if (hasContradiction reduced) then "no solution!\n"
                      else ((if (hasInfiniteSolutions reduced) then "infinite solutions:\n"
                                else "unique solution:\n")
                            ++ showEquations reduced)
  where
    reduced = rrefAug m
--}}}


{---- PARSING ----} --{{{
-- parses zero or more spaces
whitespace :: Parser ()
whitespace = skipMany (char ' ')

-- parses an integer, with optional negative sign
number :: Parser Integer
number = (do char '-'
             ds <- many1 digit
             return (-(read ds))
          ) <|>
          (do ds <- many1 digit
              return (read ds)
           )

-- parses a Val, with optional slash for fractions
-- allows the denominator to have a sign too
val :: Parser Val
val = do a <- number
         (do char '/'
             b <- number
             return (a%b)
          ) <|>
          (do return (a%1)
           )

-- parses a Row, composed of Vals separated by spaces
row :: Parser Row
row = sepEndBy1 val whitespace

-- parses an AugRow, composed of two Rows separated by a vertical bar
augRow :: Parser AugRow
augRow = do lr <- row
            char '|'
            whitespace
            rr <- row
            return (lr, rr)

-- parses a matrix, composed of one or many AugRows separated by newlines
matrix :: Parser AugMatrix
matrix = sepEndBy1 augRow newline
--}}}


{---- DO THE REAL WORK ----} --{{{
-- main method; does all IO
main = do
  putStrLn "Input rows, and an empty line when finished."
  putStrLn "Separate values by spaces,"
  putStrLn "and the coefficients from the constants by vertical lines."
  putStrLn "Example: 0 1 -2/3 4 5/-7 | 9"
  input <- readUntilEmptyLine
  case parse matrix "" input of
    Left err  -> print err -- if there was a parsing error, print it
    Right mat -> putStr $ interpretSystem mat -- otherwise interpret the system
 
-- reads lines from stdin until an empty line is given
-- uses readline, giving full line editing power
readUntilEmptyLine :: IO String
readUntilEmptyLine = do
  line <- readline "> "
  case line of
    Nothing    -> return "" -- EOF on empty line
    Just ""    -> return "" -- 
    Just line1 -> (do
                  line2 <- readUntilEmptyLine
                  return (line1 ++ "\n" ++ line2))

-- actually perform row reduction on an AugMatrix.
-- the first argument is the list of completed rows, which accumulates
-- as it calls itself recursively
reallyRrefAug :: AugMatrix -> AugMatrix -> AugMatrix
reallyRrefAug done []       = done
reallyRrefAug done (ar:ars) = reallyRrefAug (elim done ++ [leadingOneAug ar]) (elim ars)
  where
    elim = map (eliminateAug (leadingOneAug ar))
--}}}
