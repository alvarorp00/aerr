module Lib(
    mainLibFn
) where

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
data Exp = AStmt AExp  -- Arithmetic expression
    | BStmt BExp  -- Boolean expression

-- Arithmetic binary operators  --> Just for notation as they don't have a real significance, actually they can be considered the same
data ABinOp = AAdd  -- (+) operator
    | ASub  -- (-) operator
    | AMul  -- (*) operator
    deriving (Show)

-- Arithmetic expressions
data AExp = Const Int  -- Constant integer value
    | Var Char  -- Vars are constructed from single chars --> Var 'x' OK, Var "x1" NOT OK (could be easily changed but I decided it in this way)
    | AOp ABinOp AExp AExp  -- Arithmetic operation --> Posterior LV analysis will check only Var Char acts as left operator of this expression type
    deriving (Show)

-- Boolean Operators & expressions
data BExp = BLEq AExp AExp  -- Less or equal than
    | BGr AExp AExp  -- Greater than  --> is the same as negating the possible result of a BLEq expression type so I have included just to simplify nodes definition (see below)
    | BEq AExp AExp  -- Equal
    | BAnd BExp BExp  -- Boolean and
    deriving (Show)

---------------------
---- DEFINITIONS ----
---------------------

-- Define VarSet as a set of VarChar's
--      The power of using a Set type is as no repetition is allowed we ensure
--      no duplicate variables will appear
type VarSet = Set.Set Char

-- 2. Implement a function that given an arithmetic/boolean exp.
--    returns the set of variables ocurring in it.

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
                    vars'' (BGr a1 a2) = Set.union (vars' (AStmt a1)) (vars' (AStmt a2))
                    vars'' (BEq a1 a2) = Set.union (vars' (AStmt a1)) (vars' (AStmt a2))
                    vars'' (BAnd b1 b2) = Set.union (vars'' b1) (vars'' b2)

---------------------
------- CFG's -------
---------------------

-- 3. Implement CFG Data Structure

data CFGBlock = AssignBlock AExp AExp  -- need to check later that left part of assignment is Var type
    | CondBlock BExp
    deriving(Show)

data CFG = Block {
    block :: CFGBlock,
    label :: Int,
    succs :: [Int]  -- List of labels of CFGS
} deriving (Show)

---------------------
---- TRANSFER. F ----
---------------------

-- 4. Implement kill & gen functions together with a transfer function

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
--- LIVE VARIABLE ---
------ ANALYSIS -----
---------------------

-- Alias for (key,value) pair --> key as block label, value as set of vars (either LVOuts or LVIns)
type LVMap = Map.Map Int VarSet

-- Returns the block contained in a node of the graph
getBlock :: CFG -> CFGBlock
getBlock (Block b _ _) = b

-- Returns the label contained in a node of the graph
getLabel :: CFG -> Int
getLabel (Block _ l _) = l

-- Returns the labels of the successors of the corresponding node of the graph
getChildrenLabels :: CFG -> [Int]
getChildrenLabels (Block _ _ c) = c

-- Returns a map of type (label_idx, EmptySet) --> ⊥ --> First input of F function --> F(⊥)
--      @ Associates each label (as index) to an empty set (at first no values are contained in LVOut & LVIn sets)
bottom :: [Int] -> LVMap
bottom [] = Map.empty
bottom (x:xs) = Map.union (bottom xs) (Map.insert x Set.empty Map.empty)

-- Performs LiveVariableAnalysis 
--
-- F: L -> L (monotonic)
--
-- Args:
--      ~ [CFG] : set of blocks -> Blocks representing the graph flow of the program (each block contains labels of its childs a.k.a. successors)
-- Returns:
--      ~ (LVMap, LVMap) : (LVOut_n, LVIn_n) -> Tuple containing LVOut & 
--                                              LVIn of each block
f :: [CFG] -> (LVMap, LVMap)
f nodes = f' emptyVals emptyVals  -- start with F(⊥) <-- actually two parameters as LVOuts & LVIns are calculated separately
    where
        emptyVals = bottom labels  -- ⊥
        labels = map (\x -> getLabel x) (Map.elems tblocks) -- get label of each node
        tblocks = buildCFGs nodes -- build a Map where the key is the label of the node and the value the node itself
        f' :: LVMap -> LVMap -> (LVMap, LVMap)  -- From (LVOut_n, LVIn_n) calculate (LVOut_{n+1}, LVIn_{n+1})
        f' lvouts lvins
            | fixed_point = (lvouts', lvins')  -- If it's the same as in the previous iteration a fixed point has been reached
            | otherwise   = f' lvouts' lvins'  -- Recursive call to F^{n+1}(⊥) in the n-th iteration
            where
                fixed_point = (lvouts' == lvouts) && (lvins' == lvins)
                lvouts' = Map.mapWithKey (inverseTransfer tblocks lvins) lvouts  -- next LVOuts --> map the partialized function (with nodes & current lvins) to each pair (key,value) of the current lvouts ((label, VarSet) style)
                lvins' = Map.mapWithKey (transferMap tblocks lvouts) lvins -- next LVIns --> map the partialized function (with nodes & current lvouts) to each pair (key,value) of the current lvins ((label, VarSet) style)

-- 
transferMap :: Map.Map Int CFG -> LVMap -> Int -> VarSet -> VarSet
transferMap tblocks lvouts k _ = transfer cfg lvout
    where
        cfg = tblocks Map.! k
        lvout = lvouts Map.! k

-- With a given set of LVIns calculate the correspondant LVOut{i} for each LVIn{i}
inverseTransfer :: Map.Map Int CFG -> LVMap -> Int -> VarSet -> VarSet
inverseTransfer tblocks lvins k _ = varSet
    where
        varSet = (Set.unions . Map.elems) filteredLvIns  -- Adds all to a single set of VarChar
        filteredLvIns = Map.filterWithKey (\k' _ -> Set.member k' cLabels) lvins  -- 
        cLabels = (Set.fromList . getChildrenLabels) (tblocks Map.! k)  -- labels of the successors of the given block

---------------------
------- DEBUG -------
---------------------

-- Deleted (consisted of random expressions and prints) --

---------------------
----- STRINGIFY -----
---- EXPRESSIONS ----
---------------------

-- strfy is not used actually, was useful for debugging in earlier code
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
                  strfy'' (BGr a1 a2) = " (" ++ ( strfy' (AStmt a1) ) ++ ") > (" ++ ( strfy' (AStmt a2) ) ++ ") "
                  strfy'' (BEq a1 a2) = " (" ++ ( strfy' (AStmt a1) ) ++ ") = (" ++ ( strfy' (AStmt a2) ) ++ ") "
                  strfy'' (BAnd b1 b2) = " (" ++ ( strfy'' b1 ) ++ ") ^ (" ++ ( strfy'' b2 ) ++ ") "

---------------------
----- HARDCODED -----
------- BLOCKS ------
---------------------

-----------------------
------- EXAMPLES ------
-----------------------

-----------------------
---- SLIDES EXAMPLE ---
-----------------------
-------- START --------
-----------------------

-- [1] @ x:= 1  --> [2]
-- [2] @ y > 0   --> True : [3] | False : [4]
-- [3] @ x := x - 1 --> [2]
-- [4] @ x := 2 --> [END]

-- EXPRESSIONS --

exp11 :: CFGBlock
exp11 = AssignBlock (Var 'x') (Const 1) -- x := 1

exp12 :: CFGBlock
exp12 = CondBlock (BGr (Var 'y') (Const 0))  -- y > 0

exp13 :: CFGBlock
exp13 = AssignBlock (Var 'x') (AOp ASub (Var 'x') (Const 1)) -- x := x - 1

exp14 :: CFGBlock
exp14 = AssignBlock (Var 'x') (Const 2)  -- x := 2

-- NODES --

node11 :: CFG
node11 = Block {block=exp11, label=1, succs=[2]}

node12 :: CFG
node12 = Block {block=exp12, label=2, succs=[3,4]}

node13 :: CFG
node13 = Block {block=exp13, label=3, succs=[2]}

node14 :: CFG
node14 = Block {block=exp14, label=4, succs=[]}

nodes1 :: [CFG]
nodes1 = [node11, node12, node13, node14]

-----------------------
---- SLIDES EXAMPLE ---
-----------------------
--------- ENDS --------
-----------------------

-----------------------

-----------------------
--- HANDOUT EXAMPLE ---
-----------------------
-------- START --------
-----------------------

-- [1] @ x:= 10  --> [2]
-- [2] @ x > 0   --> True : [3] | False : [5]
-- [3] @ x := x - 1 --> [4]
-- [4] @ y := y + x --> [2]
-- [5] @ z := 2 --> [6]
-- [6] @ z := y * z --> [END]

-- EXPRESSIONS --

exp21 :: CFGBlock
exp21 = AssignBlock (Var 'x') (Const 10) -- x := 10

exp22 :: CFGBlock
exp22 = CondBlock (BGr (Var 'x') (Const 0)) -- x > 0

exp23 :: CFGBlock
exp23 = AssignBlock (Var 'x') (AOp ASub (Var 'x') (Const 1)) -- x := x - 1

exp24 :: CFGBlock
exp24 = AssignBlock (Var 'y') (AOp AAdd (Var 'y') (Var 'x')) -- y := y - x

exp25 :: CFGBlock
exp25 = AssignBlock (Var 'z') (Const 2) -- z := 2

exp26 :: CFGBlock
exp26 = AssignBlock (Var 'z') (AOp AMul (Var 'y') (Var 'z'))

-- NODES --

node21 :: CFG
node21 = Block {block=exp21, label=1, succs=[2]}

node22 :: CFG
node22 = Block {block=exp22, label=2, succs=[3, 5]}

node23 :: CFG
node23 = Block {block=exp23, label=3, succs=[4]}

node24 :: CFG
node24 = Block {block=exp24, label=4, succs=[2]}

node25 :: CFG
node25 = Block {block=exp25, label=5, succs=[6]}

node26 :: CFG
node26 = Block {block=exp26, label=6, succs=[]}

nodes2 :: [CFG]
nodes2 = [node21, node22, node23, node24, node25, node26]

-----------------------
--- HANDOUT EXAMPLE ---
-----------------------
--------- ENDS --------
-----------------------

---------------------
---- BUILD CFGs -----
---------------------

-- This function builds Map{key, value} e.g. Map {label, node} from above nodes
-- by mapping each cfg label to itself as a key value tuple inside a Map.Map structure
buildCFGs :: [CFG] -> Map.Map Int CFG
buildCFGs nodes = Map.fromList $ map (\x -> (getLabel x, x)) nodes

---------------------
---------------------
-------- MAIN -------
---------------------
---------------------

formatLV :: [(Int, VarSet)] -> String
formatLV [] = ""
formatLV (x:xs) = "\n\tBlock " ++ show l ++ ": {"
    ++ ((formatVars . Set.toList) varSet) ++ "}" ++ formatLV xs
    where l = fst x
          varSet = snd x
          formatVars :: [Char] -> String
          formatVars [] = ""
          formatVars (s:[]) = " " ++ show s ++ " "
          formatVars (s:ss) = " " ++ show s ++ (formatVars ss)

-- CHOOSE PROGRAM TO ANALYZE --

mainLibFn :: IO()
mainLibFn = do
    let result = f nodes2 -- change `nodesX` to desired nodes {nodes1, nodes2} to check available hardcoded programs
    let frmtLVOut = formatLV $ (Map.toList . fst) result
    let frmtLVIn = formatLV $ (Map.toList . snd) result
    putStrLn $ "LVOut (Block[i]: {LVOutVars}) : " ++ frmtLVOut
    putStrLn $ "LVIn (Block[i]: {LVInVars}) : " ++  frmtLVIn

-------------------
-------------------
---- INSTANCES ----
-------------------
-------------------

instance Show Exp where
    show = strfy

instance Eq CFG where
    c1 == c2 = label c1 == label c2

instance Ord CFG where
    c1 `compare` c2 = label c1 `compare` label c2
