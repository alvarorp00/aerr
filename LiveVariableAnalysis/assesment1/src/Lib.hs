module Lib where

import Data.Char()
import Data.Int()
-- import Data.Maybe
import Data.List()
import qualified Data.Set as Set
-- import Data.Function

-- 1. Implement abstract data types

-- Expressions
data Exp = SkipExp
    | Stmt Exp Exp
    | AStmt AExp
    | BStmt BExp

-- Arithmetic binary operators
data ABinOp = AAdd
    | ASub
    | AMul
    deriving (Show)

-- Arithmetic expressions
data AExp = Const Int
    | Var Char  -- Vars are build up from single char
    | AOp ABinOp AExp AExp
    deriving (Show)

-- Boolean Operators & expressions
data BExp = BLEq AExp AExp
    | BEq AExp AExp
    | BAnd BExp BExp
    deriving (Show)

---------------------
---- DEFINITIONS ----
---------------------

-- 2. Implement a function that given an arithmetic/boolean exp.
--    returns the set of variables ocurring in it.

-- rmdups --> remove duplicates from a list.
--            Unused since Set.fromList is being used.

rmdups :: Eq a => [a] -> [a]
rmdups []     = []
rmdups (x:xs) = x : filter (/= x) (rmdups xs)

vars :: Exp -> Set.Set Char
vars e = Set.fromList $ vars' e
    where vars' :: Exp -> [Char]
          vars' (SkipExp) = []
          vars' (Stmt s1 s2) = (vars' s1) ++ "; " ++ (vars' s2)
          vars' (AStmt a) = vars'' a
              where vars'' :: AExp -> [Char]
                    vars'' (Const _) = []
                    vars'' (Var x) = [x]
                    vars'' (AOp _ a1 a2) = ( vars' (AStmt a1) ) ++ ( vars' (AStmt a2) )
          vars' (BStmt b) = vars'' b
              where vars'' :: BExp -> [Char]
                    vars'' (BLEq a1 a2) = ( vars' (AStmt a1) ) ++ ( vars' (AStmt a2) )
                    vars'' (BEq a1 a2) = ( vars' (AStmt a1) ) ++ ( vars' (AStmt a2) )
                    vars'' (BAnd b1 b2) = (vars'' b1) ++ (vars'' b2)

---------------------
------- CFG's -------
---------------------

data CFG = CFG CFGBlockType Int -- Int references the label of the block

data CFGBlockType = SkipBlock
    | AssignBlock AExp AExp  -- need to check later that left part of assignment is Var type
    | CondBlock BExp Exp

---------------------
----- STRINGIFY -----
---------------------

strfy :: Exp -> String
strfy e = "(" ++ strfy' e ++ ")"
    where strfy' :: Exp -> String 
          strfy' (SkipExp) = ""
          strfy' (Stmt s1 s2) = (strfy' s1) ++ (strfy' s2)
          strfy' (AStmt a) = "" ++ strfy'' a ++ ""
            where strfy'' :: AExp -> String
                  strfy'' (Const i) = "" ++ ( show i ) ++ ""
                  strfy'' (Var x) = "" ++ ( show x ) ++ ""
                  strfy'' (AOp AAdd a1 a2) = " (" ++ ( strfy'' a1 ) ++ ") + (" ++ ( strfy'' a2 ) ++ ") "
                  strfy'' (AOp ASub a1 a2) = " (" ++ ( strfy'' a1 ) ++ ") - (" ++ ( strfy'' a2 ) ++ ") "
                  strfy'' (AOp AMul a1 a2) = " (" ++ ( strfy'' a1 ) ++ ") * (" ++ ( strfy'' a2 ) ++ ") "
          strfy' (BStmt b) = "" ++ strfy'' b ++ ""
            where strfy'' :: BExp -> String
                  strfy'' (BLEq a1 a2) = " (" ++ ( strfy' (AStmt a1) ) ++ ") <= (" ++ ( strfy' (AStmt a2) ) ++ ") "
                  strfy'' (BEq a1 a2) = " (" ++ ( strfy' (AStmt a1) ) ++ ") = (" ++ ( strfy' (AStmt a2) ) ++ ") "
                  strfy'' (BAnd b1 b2) = " (" ++ ( strfy'' b1 ) ++ ") ^ (" ++ ( strfy'' b2 ) ++ ") "

---------------------
---- EXPRESSIONS ----
---------------------

aexp1 :: AExp
aexp1 = AOp AMul (AOp AAdd (Var 'x') (Var 'y')) (Var 'z')

aexp2 :: AExp
aexp2 = AOp ASub (AOp AAdd (Var 'x') (Var 'y')) (Var 'y')

bexp1 :: BExp
bexp1 = BEq aexp1 aexp2

exp1 :: Exp
exp1 = AStmt aexp1

exp2 :: Exp
exp2 = AStmt aexp2

exp3 :: Exp
exp3 = BStmt bexp1


---------------------
------- TESTS -------
---------------------

mainLib :: IO()
mainLib = do
    print $ vars exp3
    -- print exp3
    -- print $ vars exp3

instance Show Exp where
    show = strfy