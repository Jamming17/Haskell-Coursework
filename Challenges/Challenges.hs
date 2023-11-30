{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2021
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (TileEdge(..),Tile(..),Puzzle,isPuzzleComplete,
                   Rotation(..),solveCircuit,
                   LExpr(..),Bind(..),prettyPrint,parseLetx,
                   LamExpr(..),letEnc,compareRedn)
                    where

-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
--import Parsing
import Data.List

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West  deriving (Eq,Ord,Show,Read)
data Tile = Source [ TileEdge ] | Sink [ TileEdge ] | Wire [ TileEdge ]  deriving (Eq,Show,Read)
type Puzzle = [ [ Tile ] ]

isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete p = isFullyConnected p

isFullyConnected :: Puzzle -> Bool
isFullyConnected p = if and [(isHorizontallyConnected p), (isVerticallyConnected (transpose p))] == True then (sourcesAndSinks 1 (concat p) (createPuzzleTuple p)) else False

--Checks whether every source connects to a sink and vice-versa
sourcesAndSinks :: Int -> [Tile] -> ([(Int, Tile)], Int, Int) -> Bool
sourcesAndSinks _ [] _ = True
sourcesAndSinks i (p:ps) pt = and [checkTile i p pt : sourcesAndSinks (i + 1) ps pt] where
  checkTile :: Int -> Tile -> ([(Int, Tile)], Int, Int) -> Bool
  checkTile _ (Wire _) _ = True
  checkTile i (Source ss) pt = containsSink (fullPath ss i pt [])
  checkTile i (Sink ss) pt = containsSource (fullPath ss i pt []) where
    containsSink :: [Tile] -> Bool
    containsSink [] = False
    containsSink ((Sink _):_) = True
    containsSink (t:ts) = containsSink ts
    containsSource :: [Tile] -> Bool
    containsSource [] = False
    containsSource ((Source _):_) = True
    containsSource (t:ts) = containsSource ts
    -- Parameters: edges of current tile / index of current tile / puzzle tuple / list of visited indexes
    fullPath :: [TileEdge] -> Int -> ([(Int, Tile)], Int, Int) -> [Int] -> [Tile]
    fullPath es i pt@(p, h, w) v = (filter (\(x, _) -> x == i) p) ++ northNode ++ southNode ++ eastNode ++ westNode where
      northNode = (if North `elem` es then (if (i - w) `elem` v then (fullPath (getTileEdges (snd (head (filter (\(x, _) -> x == (i - w)) p)))) (i - w) pt (i : v)) else []) else [])
      southNode = (if South `elem` es then (if (i + w) `elem` v then (fullPath (getTileEdges (snd (head (filter (\(x, _) -> x == (i + w)) p)))) (i + w) pt (i : v)) else []) else [])
      eastNode = (if East `elem` es then (if (i + 1) `elem` v then (fullPath (getTileEdges (snd (head (filter (\(x, _) -> x == (i + 1)) p)))) (i + 1) pt (i : v)) else []) else [])
      westNode = (if West `elem` es then (if (i - 1) `elem` v then (fullPath (getTileEdges (snd (head (filter (\(x, _) -> x == (i - 1)) p)))) (i - 1) pt (i : v)) else []) else [])

--Indexed Flattened list of tuples, height, width
createPuzzleTuple :: Puzzle -> ([(Int, Tile)], Int, Int)
createPuzzleTuple p = ((zip [1..] (concat p)), length p, (length (concat p)) `div` (length p))

--Checks whether a row of wires in the puzzle are all connected
isHorizontallyConnected :: Puzzle -> Bool
isHorizontallyConnected [] = True
isHorizontallyConnected (ts:tss) = (and [(isRowHorizontallyConnected ts True), (isHorizontallyConnected tss)]) where
  isRowHorizontallyConnected :: [Tile] -> Bool -> Bool
  isRowHorizontallyConnected [] _ = True
  isRowHorizontallyConnected at@(t:ts) True = if West `elem` (getTileEdges t) then False else isRowHorizontallyConnected at False
  isRowHorizontallyConnected (t:[]) _ = if East `elem` (getTileEdges t) then False else True
  isRowHorizontallyConnected (t1:t2:ts) _ = (and [(horizontalCheckConnect t1 t2), (isRowHorizontallyConnected (t2 : ts) False)]) where
    horizontalCheckConnect :: Tile -> Tile -> Bool
    horizontalCheckConnect t1 t2 = if East `elem` t1e && not (West `elem` t2e) || West `elem` t2e && not (East `elem` t1e) then False else True where
      t1e = getTileEdges t1
      t2e = getTileEdges t2

--Checks whether a column of wires in the puzzle are all connected
isVerticallyConnected :: Puzzle -> Bool
isVerticallyConnected [] = True
isVerticallyConnected (ts:tss) = (and [(isRowVerticallyConnected ts True), (isVerticallyConnected tss)]) where
  isRowVerticallyConnected :: [Tile] -> Bool -> Bool
  isRowVerticallyConnected [] _ = True
  isRowVerticallyConnected at@(t:ts) True = if North `elem` (getTileEdges t) then False else isRowVerticallyConnected at False
  isRowVerticallyConnected (t:[]) _ = if South `elem` (getTileEdges t) then False else True
  isRowVerticallyConnected (t1:t2:ts) _ = (and [(verticalCheckConnect t1 t2), (isRowVerticallyConnected (t2 : ts) False)]) where
    verticalCheckConnect :: Tile -> Tile -> Bool
    verticalCheckConnect t1 t2 = if South `elem` t1e && not (North `elem` t2e) || North `elem` t2e && not (South `elem` t1e) then False else True where
      t1e = getTileEdges t1
      t2e = getTileEdges t2

--Retrieves tile edges regardless of type
getTileEdges :: Tile -> [TileEdge]
getTileEdges (Wire es) = es
getTileEdges (Sink es) = es
getTileEdges (Source es) = es


-- Challenge 2
-- Solving Circuits
data Rotation = R0 | R90 | R180 | R270 
  deriving (Eq,Show,Read)

solveCircuit :: Puzzle -> Maybe [[ Rotation ]]
solveCircuit = undefined

-- Challenge 3
-- Pretty Printing Let Expressions

data LExpr = Var Int | App LExpr LExpr | Let Bind  LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr  | Abs Bind LExpr 
    deriving (Eq,Show,Read)
data Bind = Discard | V Int 
    deriving (Eq,Show,Read)

prettyPrint :: LExpr -> String
prettyPrint =  undefined


-- Challenge 4 - Parsing Let Expressions

parseLetx :: String -> Maybe LExpr
parseLetx = undefined

-- Challenge 5
-- Let Encoding in Lambda 

data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr 
                deriving (Eq, Show, Read)

letEnc :: LExpr -> LamExpr 
letEnc =  undefined

-- Challenge 6
-- Compare Innermost Reduction for Let_x and its Lambda Encoding

------------
-- LAMBDA --
------------

free :: Int -> LamExpr -> Bool
free x (LamVar y) =  x == y
free x (LamAbs y e) | x == y = False
free x (LamAbs y e) | x /= y = free x e
free x (LamApp e1 e2)  = (free x e1) || (free x e2)

rename :: Int -> LamExpr -> Int
rename x e | free (x+1) e = rename (x+1) e
           | otherwise = x+1 

subst :: LamExpr -> Int ->  LamExpr -> LamExpr
subst (LamVar x) y e | x == y = e
subst (LamVar x) y e | x /= y = LamVar x
subst (LamAbs x e1) y e  |  x /= y && not (free x e)  = LamAbs x (subst e1 y e)
subst (LamAbs x e1) y e  |  x /= y &&     (free x e)  = let x' = (rename x e1) in subst (LamAbs x' (subst e1 x (LamVar x'))) y e
subst (LamAbs x e1) y e  | x == y  = LamAbs x e1
subst (LamApp e1 e2) y e = LamApp (subst e1 y e) (subst e2 y e) 

isLamValue :: LamExpr -> Bool
isLamValue (LamVar _) = True
isLamValue (LamAbs _ _) = True
isLamValue _ = False

-- CALL BY VALUE -- 
cbvlam1 :: LamExpr -> Maybe LamExpr
-- Contexts
cbvlam1 (LamApp e1 e2) | not (isLamValue e1) = 
  do e' <- cbvlam1 e1
     return (LamApp e' e2)
cbvlam1 (LamApp e1 e2) | not (isLamValue e2) = 
  do e' <- cbvlam1 e2
     return (LamApp e1 e')
-- Reductions 
cbvlam1 (LamApp (LamAbs x e1) e) | isLamValue e = Just (subst e1 x e)
-- Otherwise terminated or blocked
cbvlam1 _ = Nothing

-- CALL BY NAME --
cbnlam1 :: LamExpr -> Maybe LamExpr
-- Reductions 
cbnlam1 (LamApp (LamAbs x e1) e) = Just (subst e1 x e)
-- Contexts
cbnlam1 (LamApp e1 e2) = 
  do e' <- cbnlam1 e1
     return (LamApp e' e2)
-- Otherwise terminated or blocked
cbnlam1 _ = Nothing

---------
-- LET --
--------- 



compareRedn :: LExpr -> Int -> (Int,Int,Int,Int)
compareRedn = undefined