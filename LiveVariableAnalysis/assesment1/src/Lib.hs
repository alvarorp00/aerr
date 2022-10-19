module Lib where

import Data.Char()
import Data.Int()
-- import Data.Maybe
import Data.List()
import qualified Data.Set as Set
-- import Data.Function

---------------------
----- LANGUAGE ------
---------------------

-- 1. Implement abstract data types

-- Expressions
data Exp = AStmt AExp
    | BStmt BExp

-- Arithmetic binary operators
data ABinOp = AAdd
    | ASub
    | AMul
    deriving (Show)

-- Arithmetic expressions
data AExp = Const Int
    | Var Char
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

-- Define VarSet as a set of VarChar's
type VarSet = Set.Set Char

-- 2. Implement a function that given an arithmetic/boolean exp.
--    returns the set of variables ocurring in it.

-- rmdups --> remove duplicates from a list.
--            Unused since Set is being used.
-- rmdups :: Eq a => [a] -> [a]
-- rmdups []     = []
-- rmdups (x:xs) = x : filter (/= x) (rmdups xs)

vars :: Exp -> VarSet
vars e = vars' e
    where vars' :: Exp -> VarSet
          vars' (AStmt a) = vars'' a
              where vars'' :: AExp -> VarSet
                    vars'' (Const _) = Set.empty
                    vars'' (Var x) = Set.insert x Set.empty
                    vars'' (AOp _ a1 a2) = Set.union (vars' (AStmt a1)) (vars' (AStmt a2))
          vars' (BStmt b) = vars'' b
              where vars'' :: BExp -> VarSet
                    vars'' (BLEq a1 a2) = Set.union (vars' (AStmt a1)) (vars' (AStmt a2))
                    vars'' (BEq a1 a2) = Set.union (vars' (AStmt a1)) (vars' (AStmt a2))
                    vars'' (BAnd b1 b2) = Set.union (vars'' b1) (vars'' b2)

---------------------
------- CFG's -------
---------------------

data CFG = CFG CFGBlockType Int deriving(Show) -- Int references the label of the block

-- data Variable a = Var a

data CFGBlockType = SkipBlock
    | AssignBlock AExp AExp  -- need to check later that left part of assignment is Var type
    | CondBlock BExp
    deriving(Show)

---------------------
---- TRANSFER. F ----
---------------------

-- Return the set of variables _killed_ in block n
kill :: CFG -> VarSet
kill (CFG block _) = kill' block
    where kill' :: CFGBlockType -> VarSet
          kill' (AssignBlock (Var v) _) = Set.insert v Set.empty -- must match Var and not any other AExp
          kill' (SkipBlock) = Set.empty
          kill' (AssignBlock _ _) = Set.empty
          kill' (CondBlock _) = Set.empty

-- Return the set of variables _read_ in block n
gen :: CFG -> VarSet
gen (CFG block _) = gen' block
    where gen' :: CFGBlockType -> VarSet
          gen' (AssignBlock (Var _) a) = vars $ AStmt a -- preserve syntax -> assign can be to only one variable
          gen' (CondBlock b) = vars $ BStmt b
          gen' (SkipBlock) = Set.empty
          gen' (AssignBlock _ _) = Set.empty

-- Return the set of live variables at the entry of block n
-- LVInn = (LVOutn − Killn) ∪ Genn
transfer :: CFG -> VarSet -> VarSet
transfer block lvout = 
    Set.union (Set.difference lvout (kill block)) (gen block)

---------------------
----- STRINGIFY -----
---------------------

strfy :: Exp -> String
strfy e = "(" ++ strfy' e ++ ")"
    where strfy' :: Exp -> String
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

test1 :: CFG
test1 = CFG (AssignBlock ((Var 'a')) (aexp1)) 1

test2 :: CFG
test2 = CFG (CondBlock bexp1) 2


---------------------
------- TESTS -------
---------------------

mainLib :: IO()
mainLib = do
    print $ vars exp3
    print exp3
    print $ vars exp3
    print test1
    print $ kill test1
    print $ kill test2
    print $ gen test1
    print $ gen test2

instance Show Exp where
    show = strfy