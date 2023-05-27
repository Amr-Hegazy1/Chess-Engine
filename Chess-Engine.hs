import Data.Char

type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])


setBoard :: Board
setBoard = 	(White,[R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
			P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)],
			[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
			P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])

visualizeBoard:: Board->IO()

visualizeBoard (p,wp,bp) = putStr ("    a    b    c    d    e    f    g    h\n8 | "  ++ (visualizeBoardHelper 'a' 8  wp bp) ++ "\n\nTurn: " ++ (show p))


visualizeBoardHelper 'h' 1 wp bp = (find 'h' 1 White wp) ++ (find 'h' 1 Black bp)

visualizeBoardHelper 'h' i wp bp = (find 'h' i White wp) ++ (find 'h' i Black bp) ++ "\n" ++ (show (i-1)) ++ " | " ++ (visualizeBoardHelper 'a' (i-1) wp bp)



visualizeBoardHelper c i wp bp = (find c i White wp) ++ (find c i Black bp) ++ " | " ++ (visualizeBoardHelper (returnNext c) i wp bp)

find _ _ _ [] = ""

find c i p (P (col,row):t) | col == c && row == i && p == White =  "PW" 
                           | col == c && row == i && p == Black =  "PB"
                           | otherwise = find c i p t

find c i p (N (col,row):t) | col == c && row == i && p == White =  "NW" 
                           | col == c && row == i && p == Black =  "NB"
                           | otherwise = find c i p t


find c i p (K (col,row):t) | col == c && row == i && p == White =  "KW" 
                           | col == c && row == i && p == Black =  "KB"
                           | otherwise = find c i p t

find c i p (Q (col,row):t) | col == c && row == i && p == White =  "QW" 
                           | col == c && row == i && p == Black =  "QB"
                           | otherwise = find c i p t

find c i p (R (col,row):t) | col == c && row == i && p == White =  "RW" 
                           | col == c && row == i && p == Black =  "RB"
                           | otherwise = find c i p t

find c i p (B (col,row):t) | col == c && row == i && p == White =  "BW" 
                           | col == c && row == i && p == Black =  "BB"
                           | otherwise = find c i p t




returnNext c = chr (ord c + 1)



move:: Piece -> Location -> Board -> Board


move (P (c1,i1)) (c2,i2) (p,wp,bp) | (taken (c2,i2) wp) || (taken (c2,i2) bp) = error "this place is already taken"
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | !isLegal = error "Illegal move for piece " + (show (P (c1,i1)))
                                   | p == White = (Black, (replace (P (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (P (c1,i1)) (c2,i2) bp))

move (N (c1,i1)) (c2,i2) (p,wp,bp) | (taken (c2,i2) wp) || (taken (c2,i2) bp) = error "this place is already taken"
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | !isLegal = error "Illegal move for piece " + (show (N (c1,i1)))
                                   | p == White = (Black, (replace (N (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (N (c1,i1)) (c2,i2) bp))


move (K (c1,i1)) (c2,i2) (p,wp,bp) | (taken (c2,i2) wp) || (taken (c2,i2) bp) = error "this place is already taken"
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | !isLegal = error "Illegal move for piece " + (show (K (c1,i1)))
                                   | p == White = (Black, (replace (K (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (K (c1,i1)) (c2,i2) bp))


move (Q (c1,i1)) (c2,i2) (p,wp,bp) | (taken (c2,i2) wp) || (taken (c2,i2) bp) = error "this place is already taken"
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | !isLegal = error "Illegal move for piece " + (show (Q (c1,i1)))
                                   | p == White = (Black, (replace (Q (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (Q (c1,i1)) (c2,i2) bp))


move (R (c1,i1)) (c2,i2) (p,wp,bp) | (taken (c2,i2) wp) || (taken (c2,i2) bp) = error "this place is already taken"
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | !isLegal = error "Illegal move for piece " + (show (R (c1,i1)))
                                   | p == White = (Black, (replace (R (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (R (c1,i1)) (c2,i2) bp))

move (B (c1,i1)) (c2,i2) (p,wp,bp) | (taken (c2,i2) wp) || (taken (c2,i2) bp) = error "this place is already taken"
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | !isLegal = error "Illegal move for piece " + (show (B (c1,i1)))
                                   | p == White = (Black, (replace (B (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (B (c1,i1)) (c2,i2) bp))


taken _ [] = False

taken (c1, i1) ((P (c2,i2)) : t) = ( c1 == c2  && i1 == i2) || (taken (c1, i1) t)

taken (c1, i1) ((N (c2,i2)) : t) = ( c1 == c2  && i1 == i2) || (taken (c1, i1) t)

taken (c1, i1) ((K (c2,i2)) : t) = ( c1 == c2  && i1 == i2) || (taken (c1, i1) t)

taken (c1, i1) ((Q (c2,i2)) : t) = ( c1 == c2  && i1 == i2) || (taken (c1, i1) t)

taken (c1, i1) ((R (c2,i2)) : t) = ( c1 == c2  && i1 == i2) || (taken (c1, i1) t)

taken (c1, i1) ((B (c2,i2)) : t) = ( c1 == c2  && i1 == i2) || (taken (c1, i1) t)




replace _ _ [] = []

replace (P (c1, i1)) (c2,i2) ((P (c3,i3)) : t ) | c3 == c1 && i3 == i1 = ((P (c2,i2)) : t )
                                                | otherwise = ((P (c3,i3)) : replace (P (c1, i1)) (c2,i2) t)

replace (N (c1, i1)) (c2,i2) ((N (c3,i3)) : t ) | c3 == c1 && i3 == i1 = ((N (c2,i2)) : t )
                                                | otherwise = ((N (c3,i3)) : replace (N (c1, i1)) (c2,i2) t)

replace (K (c1, i1)) (c2,i2) ((K (c3,i3)) : t ) | c3 == c1 && i3 == i1 = ((K (c2,i2)) : t )
                                                | otherwise = ((K (c3,i3)) : replace (K (c1, i1)) (c2,i2) t)


replace (Q (c1, i1)) (c2,i2) ((Q (c3,i3)) : t ) | c3 == c1 && i3 == i1 = ((Q (c2,i2)) : t )
                                                | otherwise = ((Q (c3,i3)) : replace (Q (c1, i1)) (c2,i2) t)

replace (R (c1, i1)) (c2,i2) ((R (c3,i3)) : t ) | c3 == c1 && i3 == i1 = ((R (c2,i2)) : t )
                                                | otherwise = ((R (c3,i3)) : replace (R (c1, i1)) (c2,i2) t)

replace (B (c1, i1)) (c2,i2) ((B (c3,i3)) : t ) | c3 == c1 && i3 == i1 = ((B (c2,i2)) : t )
                                                | otherwise = ((B (c3,i3)) : replace (B (c1, i1)) (c2,i2) t)

replace p l (h:t) = (h : replace p l t) 




-- move (N ('b',3)) ('d',4) (White, [R ('h',1),N ('g',1),B ('f',1),
-- K ('e',1), Q ('d',1),B ('c',1),N ('b',3),R ('a',1),
-- P ('h',2),P ('g',2),P ('f',2),P ('e',2),
-- P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,
-- [R ('h',8),N ('g',8),B ('f',8),K ('e',8),
-- Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
-- P ('h',7),P ('g',7),P ('f',7),P ('e',7),
-- P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
