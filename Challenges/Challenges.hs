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

-- Challenge 1
-- Testing Circuits

data TileEdge = North | East | South | West  deriving (Eq,Ord,Show,Read)
data Tile = Source [ TileEdge ] | Sink [ TileEdge ] | Wire [ TileEdge ]  deriving (Eq,Show,Read)
type Puzzle = [ [ Tile ] ]

isPuzzleComplete :: Puzzle -> Bool
isPuzzleComplete p = if (sourceSinkCount (concat p)) == True then (if isFullyConnected p == True then (sourcesAndSinks 1 (concat p) (createPuzzleTuple p)) else False) else False

sourceSinkCount :: [Tile] -> Bool
sourceSinkCount [] = False
sourceSinkCount ((Sink _):_) = True
sourceSinkCount ((Source _):_) = True
sourceSinkCount (t:ts) = sourceSinkCount ts

isFullyConnected :: Puzzle -> Bool
isFullyConnected p = and [(isHorizontallyConnected p), (isVerticallyConnected (transpose p))]

--Checks whether every source connects to a sink and vice-versa
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
solveCircuit p = solveHelper (createPuzzleTuple p) 1 p

--Parameters: rotating puzzle tuple / current index / original puzzle
solveHelper :: ([(Int, Tile)], Int, Int) -> Int -> Puzzle -> Maybe [[Rotation]]
solveHelper ([], _, _) _ _ = Nothing
solveHelper pt@(its, h, w) i op
  | isPuzzleComplete (puzzleTupleToPuzzle pt) = Just (createFinalMatrix (puzzleTupleToPuzzle pt) op)
  | i > (maximum (map fst its)) = Nothing
  | getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) == [] || and (map (`elem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [North, South, East, West]) == True = if isPuzzleComplete (puzzleTupleToPuzzle pt) then Just (createFinalMatrix (puzzleTupleToPuzzle pt) op) else (if i > (maximum (map fst its)) then Nothing else (if (isJust r1) then r1 else (Nothing)))
  | and (map (`elem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [North, South]) == True && and (map (`notElem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [East, West]) == True || and (map (`elem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [East, West]) == True && and (map (`notElem` (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])))) [North, South]) == True = if isPuzzleComplete (puzzleTupleToPuzzle pt) then Just (createFinalMatrix (puzzleTupleToPuzzle pt) op) else (if i > (maximum (map fst its)) then Nothing else (if (isJust r1) then r1 else (if (isJust r2) then r2 else (Nothing))))
  | otherwise = if (isJust r1) then r1 else (if (isJust r2) then r2 else (if (isJust r3) then r3 else (if (isJust r4) then r4 else (Nothing)))) where
    r1 = solveHelper (createPuzzleTuple (insertTileIntoPuzzle pt (swapTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) (rotateTile (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i]))) R0)) i)) (i + 1) op
    r2 = solveHelper (createPuzzleTuple (insertTileIntoPuzzle pt (swapTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) (rotateTile (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i]))) R90)) i)) (i + 1) op
    r3 = solveHelper (createPuzzleTuple (insertTileIntoPuzzle pt (swapTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) (rotateTile (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i]))) R180)) i)) (i + 1) op
    r4 = solveHelper (createPuzzleTuple (insertTileIntoPuzzle pt (swapTileEdges (snd (head [(x, y) | (x, y) <- its, x == i])) (rotateTile (getTileEdges (snd (head [(x, y) | (x, y) <- its, x == i]))) R270)) i)) (i + 1) op

createFinalMatrix :: Puzzle -> Puzzle -> [[Rotation]]
createFinalMatrix np op = rebuildRotationMatrix ((length (concat np)) `div` (length np)) (createFinalMatrixHelper (concat np) (concat op)) where
  createFinalMatrixHelper :: [Tile] -> [Tile] -> [Rotation]
  createFinalMatrixHelper [] _ = []
  createFinalMatrixHelper (t1:t1s) (t2:t2s)
    | t1 == t2 = R0 : createFinalMatrixHelper t1s t2s
    | t1 == (swapTileEdges t2 (rotateTile (getTileEdges t2) R90)) = R90 : createFinalMatrixHelper t1s t2s
    | t1 == (swapTileEdges t2 (rotateTile (getTileEdges t2) R180)) = R180 : createFinalMatrixHelper t1s t2s
    | t1 == (swapTileEdges t2 (rotateTile (getTileEdges t2) R270)) = R270 : createFinalMatrixHelper t1s t2s

rebuildRotationMatrix :: Int -> [Rotation] -> [[Rotation]]
rebuildRotationMatrix _ [] = []
rebuildRotationMatrix w rs = (fst rSplit) : (rebuildRotationMatrix w (snd rSplit)) where
  rSplit = splitAt w rs

puzzleTupleToPuzzle :: ([(Int, Tile)], Int, Int) -> Puzzle
puzzleTupleToPuzzle (its, _, w) = rebuildPuzzle w (map snd its)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

insertTileIntoPuzzle :: ([(Int, Tile)], Int, Int) -> Tile -> Int -> Puzzle
insertTileIntoPuzzle (ts, h, w) t i = rebuildPuzzle w (puzzleHelper ts t i) where
  puzzleHelper :: [(Int, Tile)] -> Tile -> Int -> [Tile]
  puzzleHelper [] _ _ = []
  puzzleHelper ((pi, t):ts) tt i = if pi == i then tt : puzzleHelper ts tt i else t : puzzleHelper ts tt i

rebuildPuzzle :: Int -> [Tile] -> Puzzle
rebuildPuzzle _ [] = []
rebuildPuzzle w ts = (fst tSplit) : (rebuildPuzzle w (snd tSplit)) where
  tSplit = splitAt w ts 

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
  | e == North = West : rotateTile es R180
  | e == East = North : rotateTile es R180
  | e == South = East : rotateTile es R180
  | e == West = South : rotateTile es R180

swapTileEdges :: Tile -> [TileEdge] -> Tile
swapTileEdges (Wire _) es = (Wire es)
swapTileEdges (Source _) es = (Source es)
swapTileEdges (Sink _) es = (Sink es)

-- Challenge 3
-- Pretty Printing Let Expressions

data LExpr = Var Int | App LExpr LExpr | Let Bind  LExpr LExpr | Pair LExpr LExpr | Fst LExpr | Snd LExpr  | Abs Bind LExpr 
    deriving (Eq,Show,Read)
data Bind = Discard | V Int 
    deriving (Eq,Show,Read)

prettyPrint :: LExpr -> String
prettyPrint (Var i) = 'x' : (show i)
prettyPrint (App v@(Var _) e2) = (prettyPrint v) ++ " " ++ (prettyPrint e2)
prettyPrint (App e1 e2) = "(" ++ (prettyPrint e1) ++ ") " ++ (prettyPrint e2)
prettyPrint (Pair e1 e2) = "(" ++ (prettyPrint e1) ++ ", " ++ (prettyPrint e2) ++ ")"
prettyPrint (Fst e) = "fst (" ++ (prettyPrint e) ++ ")"
prettyPrint (Snd e) = "snd (" ++ (prettyPrint e) ++ ")"
prettyPrint a@(Abs _ _) = "\\" ++ (prettyPrintAbs a "->")
prettyPrint (Let b a@(Abs _ _) e2) = "let " ++ (prettyBind b) ++ " " ++ (prettyPrintAbs a "=") ++ " in " ++ (prettyPrint e2)
prettyPrint (Let b e1 e2) = "let " ++ (prettyBind b) ++ " = " ++ (prettyPrint e1) ++ " in " ++ (prettyPrint e2)

prettyPrintAbs :: LExpr -> String -> String
prettyPrintAbs (Abs b a@(Abs _ _)) s = (prettyBind b) ++ " " ++ (prettyPrintAbs a s)
prettyPrintAbs (Abs b e) s = (prettyBind b) ++ " " ++ s ++ " " ++ (prettyPrint e)

prettyBind :: Bind -> String
prettyBind Discard = "_"
prettyBind (V i) = 'x' : (show i)

-- Challenge 4 - Parsing Let Expressions

parseLetx :: String -> Maybe LExpr
parseLetx s = let p = parse plexpr s in
  if p == [] then Nothing else (if snd (head p) /= "" then Nothing else Just (fst (head p)))

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

plexpr :: Parser LExpr
plexpr = pbrackets `chainl1` pspace <|> pbrackets

pbrackets :: Parser LExpr
pbrackets = do char '(';
               e <- plexpr;
               char ')';
               return e
            <|> pabs

pspace :: Parser (LExpr -> LExpr -> LExpr)
pspace = do char ' ';
            return App

papp :: Parser LExpr
papp = do es <- papp `chainl1` pspace
          return es
       <|> pabs

pabs :: Parser LExpr
pabs = do char '\\';
          bs <- many1 (do space; pbind);
          string " -> ";
          e <- plexpr;
          return (foldr Abs e bs)
       <|> plet

plet :: Parser LExpr
plet = do string "let ";
          b <- pbind;
          bs <- many (do space; pbind);
          string " = ";
          e1 <- plexpr;
          string " in ";
          e2 <- plexpr;
          let fes = case bs of
                      [] -> e1
                      _  -> foldr Abs e1 bs
          return (Let b fes e2) 
       <|> ppair

ppair :: Parser LExpr
ppair = do char '(';
           e1 <- plexpr;
           char ',';
           e2 <- plexpr;
           char ')';
           return (Pair e1 e2)
         <|> pfst

pfst :: Parser LExpr
pfst = do string "fst (";
          e <- plexpr;
          char ')';
          return (Fst e)
       <|> psnd

psnd :: Parser LExpr
psnd = do string "snd (";
          e <- plexpr;
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