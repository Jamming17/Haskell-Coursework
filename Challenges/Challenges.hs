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
import Parsing

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West  deriving (Eq,Ord,Show,Read)
data Tile = Source [ TileEdge ] | Sink [ TileEdge ] | Wire [ TileEdge ]  deriving (Eq,Show,Read)
type Puzzle = [ [ Tile ] ]

isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete = undefined

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