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
import Data.List

{- 
Author    : Jack Chiplin (jc16g22)
Copyright : (c) University of Southampton

many1 and chainl1 functions adapted from "Functional Pearls: Monadic Parsing in Haskell" by G. Hutton and E Meijer
-}

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West  deriving (Eq,Ord,Show,Read)
data Tile = Source [ TileEdge ] | Sink [ TileEdge ] | Wire [ TileEdge ]  deriving (Eq,Show,Read)
type Puzzle = [ [ Tile ] ]

isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete p = if (sourceSinkCount (concat p)) == True then (if isFullyConnected p == True then (sourcesAndSinks 1 (concat p) (createPuzzleTuple p)) else False) else False

-- Checks whether there is at least one source and one sink present in the puzzle
sourceSinkCount :: [Tile] -> Bool
sourceSinkCount [] = False
sourceSinkCount ((Sink _):_) = True
sourceSinkCount ((Source _):_) = True
sourceSinkCount (t:ts) = sourceSinkCount ts

isFullyConnected :: Puzzle -> Bool
isFullyConnected p = and [(isHorizontallyConnected p), (isVerticallyConnected (transpose p))]

-- Checks whether every source connects to a sink and vice-versa
sourcesAndSinks :: Int -> [Tile] -> ([(Int, Tile)], Int, Int) -> Bool
sourcesAndSinks _ [] _ = True
sourcesAndSinks i (p:ps) pt = and (checkTile i p pt : [sourcesAndSinks (i + 1) ps pt]) where
  checkTile :: Int -> Tile -> ([(Int, Tile)], Int, Int) -> Bool
  checkTile _ (Wire _) _ = True
  checkTile i (Source ss) pt = containsSink (fullPath ss i pt []) where
    containsSink :: [Tile] -> Bool
    containsSink [] = False
    containsSink ((Sink _):_) = True
    containsSink (t:ts) = containsSink ts
  checkTile i (Sink ss) pt = containsSource (fullPath ss i pt []) where
    containsSource :: [Tile] -> Bool
    containsSource [] = False
    containsSource ((Source _):_) = True
    containsSource (t:ts) = containsSource ts

-- Parameters: edges of current tile / index of current tile / puzzle tuple / list of visited indexes
fullPath :: [TileEdge] -> Int -> ([(Int, Tile)], Int, Int) -> [Int] -> [Tile]
fullPath es i pt@(p, h, w) v = (snd (head (filter (\(x, _) -> x == i) p))) : northNode ++ southNode ++ eastNode ++ westNode where
  northNode = (if North `elem` es then (if not((i - w) `elem` v) then (fullPath (getTileEdges (snd (head (filter (\(x, _) -> x == (i - w)) p)))) (i - w) pt (i : v)) else []) else [])
  southNode = (if South `elem` es then (if not((i + w) `elem` v) then (fullPath (getTileEdges (snd (head (filter (\(x, _) -> x == (i + w)) p)))) (i + w) pt (i : v)) else []) else [])
  eastNode = (if East `elem` es then (if not((i + 1) `elem` v) then (fullPath (getTileEdges (snd (head (filter (\(x, _) -> x == (i + 1)) p)))) (i + 1) pt (i : v)) else []) else [])
  westNode = (if West `elem` es then (if not((i - 1) `elem` v) then (fullPath (getTileEdges (snd (head (filter (\(x, _) -> x == (i - 1)) p)))) (i - 1) pt (i : v)) else []) else [])

-- Indexed Flattened list of tuples, height, width
createPuzzleTuple :: Puzzle -> ([(Int, Tile)], Int, Int)
createPuzzleTuple p = ((zip [1..] (concat p)), length p, (length (concat p)) `div` (length p))

-- Checks whether a row of wires in the puzzle are all connected
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

-- Checks whether a column of wires in the puzzle are all connected
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

-- Retrieves tile edges regardless of type
getTileEdges :: Tile -> [TileEdge]
getTileEdges (Wire es) = es
getTileEdges (Sink es) = es
getTileEdges (Source es) = es


-- Challenge 2
-- Solving Circuits
data Rotation = R0 | R90 | R180 | R270 
  deriving (Eq,Show,Read)

solveCircuit :: Puzzle -> Maybe [[ Rotation ]]
solveCircuit p = solveHelper (createPuzzleTuple p) 1 p

-- Parameters: rotating puzzle tuple / current index / original puzzle
solveHelper :: ([(Int, Tile)], Int, Int) -> Int -> Puzzle -> Maybe [[Rotation]]
solveHelper ([], _, _) _ _ = Nothing
solveHelper pt@(its, h, w) i op
  | isPuzzleComplete (puzzleTupleToPuzzle pt) = Just (createFinalMatrix (puzzleTupleToPuzzle pt) op)
  | i > (maximum (map fst its)) = Nothing
    -- Checks whether the tile has no edges or all four edges
  | getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) == [] || and (map (`elem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [North, South, East, West]) == True = if isPuzzleComplete (puzzleTupleToPuzzle pt) then Just (createFinalMatrix (puzzleTupleToPuzzle pt) op) else (if i > (maximum (map fst its)) then Nothing else (if (isJust r1) then r1 else (Nothing)))
    -- Checks whether the tile has two edges facing away from each other (a straight line)
  | and (map (`elem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [North, South]) == True && and (map (`notElem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [East, West]) == True || and (map (`elem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [East, West]) == True && and (map (`notElem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [North, South]) == True = if isPuzzleComplete (puzzleTupleToPuzzle pt) then Just (createFinalMatrix (puzzleTupleToPuzzle pt) op) else (if i > (maximum (map fst its)) then Nothing else (if (isJust r1) then r1 else (if (isJust r2) then r2 else (Nothing))))
    -- Otherwise, move onto the next tile and try every puzzle possibility with all rotations of this tile
  | otherwise = if (isJust r1) then r1 else (if (isJust r2) then r2 else (if (isJust r3) then r3 else (if (isJust r4) then r4 else (Nothing)))) where
    r1 = solveHelper (createPuzzleTuple (insertTileIntoPuzzle pt (swapTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) (rotateTile (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i]))) R0)) i)) (i + 1) op
    r2 = solveHelper (createPuzzleTuple (insertTileIntoPuzzle pt (swapTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) (rotateTile (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i]))) R90)) i)) (i + 1) op
    r3 = solveHelper (createPuzzleTuple (insertTileIntoPuzzle pt (swapTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) (rotateTile (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i]))) R180)) i)) (i + 1) op
    r4 = solveHelper (createPuzzleTuple (insertTileIntoPuzzle pt (swapTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) (rotateTile (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i]))) R270)) i)) (i + 1) op

-- Turns a completed puzzle into a rotation matrix based on the original puzzle
createFinalMatrix :: Puzzle -> Puzzle -> [[Rotation]]
createFinalMatrix np op = rebuildRotationMatrix ((length (concat np)) `div` (length np)) (createFinalMatrixHelper (concat np) (concat op)) where
  createFinalMatrixHelper :: [Tile] -> [Tile] -> [Rotation]
  createFinalMatrixHelper [] _ = []
  createFinalMatrixHelper (t1:t1s) (t2:t2s)
    | t1 == t2 = R0 : createFinalMatrixHelper t1s t2s
    | t1 == (swapTileEdges t2 (rotateTile (getTileEdges t2) R90)) = R90 : createFinalMatrixHelper t1s t2s
    | t1 == (swapTileEdges t2 (rotateTile (getTileEdges t2) R180)) = R180 : createFinalMatrixHelper t1s t2s
    | t1 == (swapTileEdges t2 (rotateTile (getTileEdges t2) R270)) = R270 : createFinalMatrixHelper t1s t2s

-- Turns a list of rotations back into a full matrix
rebuildRotationMatrix :: Int -> [Rotation] -> [[Rotation]]
rebuildRotationMatrix _ [] = []
rebuildRotationMatrix w rs = (fst rSplit) : (rebuildRotationMatrix w (snd rSplit)) where
  rSplit = splitAt w rs

-- Converts a 'puzzle tuple' back into a puzzle
puzzleTupleToPuzzle :: ([(Int, Tile)], Int, Int) -> Puzzle
puzzleTupleToPuzzle (its, _, w) = rebuildPuzzle w (map snd its)

-- Detects if a Maybe is a Just or a Nothing
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- Replaces a tile of a puzzle with another tile
insertTileIntoPuzzle :: ([(Int, Tile)], Int, Int) -> Tile -> Int -> Puzzle
insertTileIntoPuzzle (ts, h, w) t i = rebuildPuzzle w (puzzleHelper ts t i) where
  puzzleHelper :: [(Int, Tile)] -> Tile -> Int -> [Tile]
  puzzleHelper [] _ _ = []
  puzzleHelper ((pi, t):ts) tt i = if pi == i then tt : puzzleHelper ts tt i else t : puzzleHelper ts tt i

-- Turns a lits of tiles back into a full puzzle
rebuildPuzzle :: Int -> [Tile] -> Puzzle
rebuildPuzzle _ [] = []
rebuildPuzzle w ts = (fst tSplit) : (rebuildPuzzle w (snd tSplit)) where
  tSplit = splitAt w ts 

-- Rotates a tile using its TileEdges
rotateTile :: [TileEdge] -> Rotation -> [TileEdge]
rotateTile [] _ = []
rotateTile es R0 = es
rotateTile (e:es) R90
  | e == North = East : rotateTile es R90
  | e == East = South : rotateTile es R90
  | e == South = West : rotateTile es R90
  | e == West = North : rotateTile es R90
rotateTile (e:es) R180  
  | e == North = South : rotateTile es R180
  | e == East = West : rotateTile es R180
  | e == South = North : rotateTile es R180
  | e == West = East : rotateTile es R180
rotateTile (e:es) R270
  | e == North = West : rotateTile es R270
  | e == East = North : rotateTile es R270
  | e == South = East : rotateTile es R270
  | e == West = South : rotateTile es R270

-- Swaps a tile's edges for another input
swapTileEdges :: Tile -> [TileEdge] -> Tile
swapTileEdges (Wire _) es = (Wire es)
swapTileEdges (Source _) es = (Source es)
swapTileEdges (Sink _) es = (Sink es)

-- Challenge 3
-- Pretty Printing Let Expressions

data LExpr = Var Int | App LExpr LExpr | Let Bind LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr  | Abs Bind LExpr 
    deriving (Eq,Show,Read)
data Bind = Discard | V Int 
    deriving (Eq,Show,Read)

-- Pretty prints a LExpr using pattern matching
prettyPrint :: LExpr -> String
prettyPrint (Var i) = 'x' : (show i)
prettyPrint (App v@(Var _) e2) = (prettyPrint v) ++ " " ++ (prettyPrint e2)
prettyPrint (App e1 e2) = "(" ++ (prettyPrint e1) ++ ") " ++ (prettyPrint e2)
prettyPrint (Pair e1 e2) = "(" ++ (prettyPrint e1) ++ ", " ++ (prettyPrint e2) ++ ")"
prettyPrint (Fst (Pair e1 e2)) = "fst (" ++ (prettyPrint e1) ++ ", " ++ (prettyPrint e2) ++ ")"
prettyPrint (Snd (Pair e1 e2)) = "snd (" ++ (prettyPrint e1) ++ ", " ++ (prettyPrint e2) ++ ")"
prettyPrint (Fst e) = "fst (" ++ (prettyPrint e) ++ ")"
prettyPrint (Snd e) = "snd (" ++ (prettyPrint e) ++ ")"
prettyPrint a@(Abs _ _) = "\\" ++ (prettyPrintAbs a "->")
prettyPrint (Let b a@(Abs _ _) e2) = "let " ++ (prettyBind b) ++ " " ++ (prettyPrintAbs a "=") ++ " in " ++ (prettyPrint e2)
prettyPrint (Let b e1 e2) = "let " ++ (prettyBind b) ++ " = " ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)

-- A separate function to deal with nested abstractions
prettyPrintAbs :: LExpr -> String -> String
prettyPrintAbs (Abs b a@(Abs _ _)) s = (prettyBind b) ++ " " ++ (prettyPrintAbs a s)
prettyPrintAbs (Abs b e) s = (prettyBind b) ++ " " ++ s ++ " " ++ (prettyPrint e)

-- A separate function to deal with binds
prettyBind :: Bind -> String
prettyBind Discard = "_"
prettyBind (V i) = 'x' : (show i)

-- Challenge 4 - Parsing Let Expressions

-- Function that parses expressions
parseLetx :: String -> Maybe LExpr
parseLetx s = let p = parse plexpr s in
  if p == [] then Nothing else (if snd (head p) /= "" then Nothing else Just (fst (head p)))

-- Following functions are adapted from "Functional Pearls: Monadic Parsing in Haskell" by G. Hutton and E Meijer 
many1 :: Parser a -> Parser [a]
many1 p = do v <- p;
             vs <- many p;
             return (v:vs)

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
                 where
                   rest a = (do f <- op
                                b <- p
                                rest (f a b))
                           <|> return a
-- End of adapted code. The following code is my own.

-- Overall parser for LExprs
plexpr :: Parser LExpr
plexpr = do space;
            e <- pbrackets `chainl1` pspace <|> pbrackets;
            space;
            return e

pbrackets :: Parser LExpr
pbrackets = do char '(';
               space;
               e <- plexpr;
               space;
               char ')';
               return e
            <|> pabs

pspace :: Parser (LExpr -> LExpr -> LExpr)
pspace = do space;
            return App

papp :: Parser LExpr
papp = do es <- papp `chainl1` pspace
          return es
       <|> pabs

pabs :: Parser LExpr
pabs = do char '\\';
          bs <- many1 (do space; pbind);
          space;
          string "->";
          space;
          e <- plexpr;
          return (foldr Abs e bs)
       <|> plet

plet :: Parser LExpr
plet = do string "let";
          space;
          b <- pbind;
          space;
          bs <- many (do space; pbind);
          space;
          char '=';
          space;
          e1 <- plexpr;
          space;
          string "in";
          space;
          e2 <- plexpr;
          let fes = case bs of
                      [] -> e1
                      _  -> foldr Abs e1 bs
          return (Let b fes e2) 
       <|> ppair

ppair :: Parser LExpr
ppair = do char '(';
           space;
           e1 <- plexpr;
           space;
           char ',';
           space;
           e2 <- plexpr;
           space;
           char ')';
           return (Pair e1 e2)
         <|> pfst

pfst :: Parser LExpr
pfst = do string "fst";
          space;
          char '('
          space;
          e <- plexpr;
          space;
          char ')';
          return (Fst e)
       <|> psnd

psnd :: Parser LExpr
psnd = do string "snd";
          space;
          char '(';
          space;
          e <- plexpr;
          space;
          char ')';
          return (Snd e)
       <|> pvar

pvar :: Parser LExpr
pvar = do char 'x';
          ns <- many1 (do digit);
          return (Var (read ns))

pbind :: Parser Bind
pbind = do char '_';
           return Discard
        <|> do char 'x';
               ns <- many1 (do digit);
               return (V (read ns))

-- Challenge 5
-- Let Encoding in Lambda 

data LamExpr = LamVar Int | LamApp LamExpr LamExpr | LamAbs Int LamExpr 
                deriving (Eq, Show, Read)

letEnc ::LExpr -> LamExpr
letEnc l = encode l

-- Encodes LExprs into LamExprs
encode :: LExpr -> LamExpr
encode (Var i) = LamVar i
encode (App e1 e2) = LamApp (encode e1) (encode e2)
encode (Abs (V i) e) = LamAbs i (encode e)
encode (Abs (Discard) e) = LamAbs (chooseDiscardVar (encode e) 0) (encode e)
encode (Let (V i) e1 e2) = LamApp (LamAbs (chooseDiscardVar (encode e2) 0) (encode e2)) (encode e1)
encode (Let (Discard) e1 e2) = LamApp (LamAbs (chooseDiscardVar (encode e2) 0) (encode e2)) (encode e1)
encode (Fst e) = LamApp (encode e) (LamAbs 0 (LamAbs 1 (LamVar 0)))
encode (Snd e) = LamApp (encode e) (LamAbs 0 (LamAbs 1 (LamVar 1)))
encode (Pair e1 e2) = LamAbs c (LamApp (LamApp (LamVar c) (encode e1)) (encode e2)) where
  c = chooseDoubleDiscardVar (encode e1) (encode e2) 0

-- Selects unused variables so that variables to not interfere with each other
selectVar :: LamExpr -> Int -> Bool
selectVar (LamVar int) i = if int == i then False else True
selectVar (LamApp e1 e2) i = (selectVar e1 i) && (selectVar e2 i)
selectVar (LamAbs int e) i = if int == i then False else (selectVar e i)

-- Selects an unused variable for the Discard token in Abs and Let expressions
chooseDiscardVar :: LamExpr -> Int -> Int
chooseDiscardVar e i = if (selectVar e i) == True then i else (chooseDiscardVar e (i + 1))

-- Selects an unused variable for the Discard token in Pair expressions
chooseDoubleDiscardVar :: LamExpr -> LamExpr -> Int -> Int
chooseDoubleDiscardVar e1 e2 i = if ((selectVar e1 i) && (selectVar e2 i)) == True then i else (chooseDoubleDiscardVar e1 e2 (i + 1))




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

substCount :: LamExpr -> Int -> LamExpr -> Int -> (LamExpr, Int)
substCount (LamVar x) y e i | x == y = (e, i + 1)
substCount (LamVar x) y e i | x /= y = (LamVar x, i + 1)
substCount (LamAbs x e1) y e i |  x /= y && not (free x e)  = (LamAbs x s, i + ic + 1) where (s, ic) = substCount e1 y e 0
substCount (LamAbs x e1) y e i |  x /= y &&     (free x e)  =(s, i + ic + 1) where (s, ic) = substCount (LamAbs x' (subst e1 x (LamVar x'))) y e 0 where x' = (rename x e1)
substCount (LamAbs x e1) y e i | x == y  = (LamAbs x e1, i + 1)
substCount (LamApp e1 e2) y e i = (LamApp s1 s2, i + ic1 + ic2 + 1)   where
  (s1, ic1) = (substCount e1 y e 0)
  (s2, ic2) = (substCount e2 y e 0)

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


cbvLamCount :: LamExpr -> Int -> Maybe (LamExpr, Int)
-- Contexts
cbvLamCount (LamApp e1 e2) i | not (isLamValue e1) =
  do (e', ic) <- cbvLamCount e1 0
     return ((LamApp e' e2), i + ic)
cbvLamCount (LamApp e1 e2) i | not (isLamValue e2) =
  do (e', ic) <- cbvLamCount e2 0
     return ((LamApp e1 e'), i + ic)
-- Reductions
cbvLamCount (LamApp (LamAbs x e1) e) i | isLamValue e = Just (s, i + ic + 1) where (s, ic) = substCount e1 x e 0
-- Otherwise terminated or blocked
cbvLamCount _ _ = Nothing

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

cbnLamCount :: Maybe LamExpr -> Int
cbnLamCount Nothing = 0
cbnLamCount (Just e) = 1 + cbnLamCount (cbnlam1 e)

unJust :: Maybe LamExpr -> LamExpr
unJust (Just l) = l

cbvLam :: LamExpr -> Int -> Int
cbvLam l i = let c = cbvlam1 l in if c == Nothing then i else cbvLam (unJust c) (i + 1)

compareRedn :: LExpr -> Int -> (Int,Int,Int,Int)
compareRedn e i = (1, 1, (cbnLamCount (Just (letEnc e))), 1)