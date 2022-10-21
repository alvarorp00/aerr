{-# LANGUAGE ScopedTypeVariables #-}

module Lib where

import Data.Char()
import Data.Int()
import Data.List()
import Data.Tuple()
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map

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

data CFG = Block {
    block :: CFGBlock,
    label :: Int,
    succs :: [CFG]
} deriving (Show)

data CFGBlock = AssignBlock AExp AExp  -- need to check later that left part of assignment is Var type
    | CondBlock BExp
    deriving(Show)

---------------------
---- TRANSFER. F ----
---------------------

-- Return the set of variables _killed_ in block n
kill :: CFG -> VarSet
kill (Block b _ _) = kill' b
    where kill' :: CFGBlock -> VarSet
          kill' (AssignBlock (Var v) _) = Set.insert v Set.empty -- must match Var and not any other AExp
          kill' (AssignBlock _ _) = Set.empty
          kill' (CondBlock _) = Set.empty

-- Return the set of variables _read_ in block n
gen :: CFG -> VarSet
gen (Block b _ _) = gen' b
    where gen' :: CFGBlock -> VarSet
          gen' (AssignBlock (Var _) a) = vars $ AStmt a -- preserve syntax -> assign can be to only one variable
          gen' (CondBlock bS) = vars $ BStmt bS
          gen' (AssignBlock _ _) = Set.empty

-- Return the set of live variables at the entry of block n
--      LVInn = (LVOutn − Killn) ∪ Genn
transfer :: CFG -> VarSet -> VarSet
transfer b lvout = 
    Set.union (Set.difference lvout (kill b)) (gen b)

---------------------
-------- LVA --------
---------------------

getBlock :: CFG -> CFGBlock
getBlock (Block b _ _) = b

getLabel :: CFG -> Int
getLabel (Block _ l _) = l

getChildren :: CFG -> [CFG]
getChildren (Block _ _ c) = c

getChildrenLabels :: CFG -> [Int]
getChildrenLabels c =
    map getLabel (getChildren c)

uniqueBlocks :: CFG -> Map.Map Int CFG
uniqueBlocks cblock =
    Map.union (Map.insert (getLabel cblock) cblock Map.empty) ((uniqueBlocks' . getChildren) cblock)
        where uniqueBlocks' :: [CFG] -> Map.Map Int CFG
              uniqueBlocks' [] = Map.empty
              uniqueBlocks' (s:ss) = Map.union (uniqueBlocks s) (uniqueBlocks' ss)

type LVMap = Map.Map Int VarSet

bottom :: [Int] -> LVMap
bottom [] = Map.empty
bottom (x:xs) = Map.union (bottom xs) (Map.insert x Set.empty Map.empty)

-- Performs LiveVariableAnalysis 
--
-- F: L -> L (monotonic)
--
-- Args:
--      ~ CFG : block -> First block of the program in the exec. path.
-- Returns:
--      ~ (LVMap, LVMap) : (LVOut_n, LVIn_n) -> Tuple containing LVOut & 
--                                              LVIn of each block
f :: CFG -> (LVMap, LVMap)
f input = f' emptyVals emptyVals
    where
        emptyVals = bottom labels
        labels = map (\x -> getLabel x) (Map.elems tblocks)
        tblocks = uniqueBlocks input
        f' :: LVMap -> LVMap -> (LVMap, LVMap)
        f' lvouts lvins = 
            if fixed_point then
                (lvouts', lvins')
            else
                f' lvouts' lvins'
            where
                fixed_point = (lvouts' == lvouts) && (lvins' == lvins)
                lvouts' = Map.mapWithKey (inverseTransfer tblocks lvins) lvouts
                lvins' = Map.mapWithKey (transferMap tblocks lvouts) lvins

transferMap :: Map.Map Int CFG -> LVMap -> Int -> VarSet -> VarSet
transferMap tblocks lvouts k _ = transfer cfg lvout
    where
        cfg = tblocks Map.! k
        lvout = lvouts Map.! k

inverseTransfer :: Map.Map Int CFG -> LVMap -> Int -> VarSet -> VarSet
inverseTransfer tblocks lvins k _ = varSet
    where
        varSet = (Set.unions . Map.elems) filteredLvIns
        filteredLvIns = Map.filterWithKey (\k' _ -> Set.member k' cLabels) lvins
        cLabels = (Set.fromList . getChildrenLabels) (tblocks Map.! k)

---------------------
------- DEBUG -------
---------------------
----- STRINGIFY -----
---- EXPRESSIONS ----
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
------- DEBUG -------
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
test1 = Block (AssignBlock ((Var 'a')) (aexp1)) 1 []

test2 :: CFG
test2 = Block (CondBlock bexp1) 2 []


---------------------
------- TESTS -------
---------------------

mainLib :: IO()
mainLib = do
    print "Exec."
    -- print $ vars exp3
    -- print exp3
    -- print $ vars exp3
    -- print test1
    -- print $ (Set.toList . kill) test1
    -- print $ (Set.toList . kill) test2
    -- print $ (Set.toList . gen) test1
    -- print $ (Set.toList . gen) test2
    -- print $ bottom [1,2,6,3,5]

instance Show Exp where
    show = strfy

instance Eq CFG where
    c1 == c2 = label c1 == label c2

instance Ord CFG where
    c1 `compare` c2 = label c1 `compare` label c2