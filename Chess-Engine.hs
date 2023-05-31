import Data.Char
import Text.Printf


type Location = (Char, Int)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location | B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

data Direction = LEFT | RIGHT | UP | DOWN | DIAGONAL_UP_RIGHT | DIAGONAL_UP_LEFT | DIAGONAL_DOWN_RIGHT | DIAGONAL_DOWN_LEFT deriving Eq


setBoard :: Board
setBoard = 	(White,[R ('h',1),N ('g',1),B ('f',1),K ('e',1),Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
			P ('h',2),P ('g',2),P ('f',2),P ('e',2),P ('d',2),P ('c',2),P ('b',2),P ('a',2)],
			[R ('h',8),N ('g',8),B ('f',8),K ('e',8),Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
			P ('h',7),P ('g',7),P ('f',7),P ('e',7),P ('d',7),P ('c',7),P ('b',7),P ('a',7)])




visualizeBoard:: Board->IO()

visualizeBoard (p,wp,bp) = putStr (printf ("    a    b    c    d    e    f    g    h\n8|"  ++ (visualizeBoardHelper 'a' 8  wp bp) ++ "\n\nTurn: " ++ (show p)))


visualizeBoardHelper 'h' 1 wp bp = (find 'h' 1 White wp) ++ (find 'h' 1 Black bp)

visualizeBoardHelper 'h' i wp bp = (find 'h' i White wp) ++ (find 'h' i Black bp) ++ "\n" ++ (show (i-1)) ++ "|" ++ (visualizeBoardHelper 'a' (i-1) wp bp)



visualizeBoardHelper c i wp bp = (find c i White wp) ++ (find c i Black bp) ++ "|" ++ (visualizeBoardHelper (returnNext c) i wp bp)

find _ _ White [] = ""
find _ _ Black [] = ""

find c i p (P (col,row):t) | col == c && row == i && p == White =  " PW " 
                           | col == c && row == i && p == Black =  " PB "
                           | otherwise = find c i p t

find c i p (N (col,row):t) | col == c && row == i && p == White =  " NW " 
                           | col == c && row == i && p == Black =  " NB "
                           | otherwise = find c i p t


find c i p (K (col,row):t) | col == c && row == i && p == White =  " KW " 
                           | col == c && row == i && p == Black =  " KB "
                           | otherwise = find c i p t

find c i p (Q (col,row):t) | col == c && row == i && p == White =  " QW " 
                           | col == c && row == i && p == Black =  " QB "
                           | otherwise = find c i p t

find c i p (R (col,row):t) | col == c && row == i && p == White =  " RW " 
                           | col == c && row == i && p == Black =  " RB "
                           | otherwise = find c i p t

find c i p (B (col,row):t) | col == c && row == i && p == White =  " BW " 
                           | col == c && row == i && p == Black =  " BB "
                           | otherwise = find c i p t




returnNext c = chr (ord c + 1)

returnPrevious c = chr (ord c - 1)



{-adam start-}
friendly _ _ White [] _ = False
friendly _ _ Black _ [] = False

friendly c i White (P (col,row):t) blackP	| col == c && row == i =  True 
											| otherwise = friendly c i White t blackP

friendly c i White (N (col,row):t) blackP	| col == c && row == i = True 
											| otherwise = friendly c i White t blackP

friendly c i White (K (col,row):t) blackP	| col == c && row == i =  True 
											| otherwise = friendly c i White t blackP

friendly c i White (Q (col,row):t) blackP	| col == c && row == i =  True 
											| otherwise = friendly c i White t blackP

friendly c i White (R (col,row):t) blackP	| col == c && row == i =  True 
											| otherwise = friendly c i White t blackP

friendly c i White (B (col,row):t) blackP  	| col == c && row == i =  True 
											| otherwise = friendly c i White t blackP



friendly c i Black whiteP (P (col,row):t) 	| col == c && row == i =  True 
											| otherwise = friendly c i Black whiteP t

friendly c i Black whiteP (N (col,row):t)	| col == c && row == i = True 
											| otherwise = friendly c i Black whiteP t

friendly c i Black whiteP (K (col,row):t)	| col == c && row == i =  True 
											| otherwise = friendly c i Black whiteP t

friendly c i Black whiteP (Q (col,row):t)	| col == c && row == i =  True 
											| otherwise = friendly c i Black whiteP t

friendly c i Black whiteP (R (col,row):t)	| col == c && row == i =  True 
											| otherwise = friendly c i Black whiteP t

friendly c i Black whiteP (B (col,row):t) 	| col == c && row == i =  True 
											| otherwise = friendly c i Black whiteP t
																		
checkSame piece  [] = False
checkSame piece  (piece1:t) = if(piece==piece1) then True
												else checkSame piece t
					
{-Returns false if there is an opponent/friendly in the path taken or the path taken is not valid-}
checkPath (col1,row1) (whiteP,blackP) (col2,row2) = if(abs( charDiff col1 col2 ) == abs( row2 - row1 )) then checkDiag (col1,row1) (whiteP,blackP) (col2,row2)
													else if((row1 == row2 && not (col1 == col2))) then checkCol (col1,row1) (whiteP,blackP) (col2,row2)
													else if((not (row1 == row2) && col1 == col2)) then checkRow (col1,row1) (whiteP,blackP) (col2,row2)
													else False
														
checkDiag (col1, row1) (whiteP, blackP) (col2, row2)| row2 > row1 && col2 > col1 = (row1 + 1 == row2 && returnNext col1==col2) || not (taken (returnNext col1, row1 + 1) whiteP || taken (returnNext col1, row1 + 1) blackP) && checkDiag (returnNext col1, row1 + 1) (whiteP, blackP) (col2, row2)
													| row2 > row1 && col2 < col1 = (row1 + 1 == row2 && returnPrevious col1==col2) || not (taken (returnPrevious col1, row1 + 1) whiteP || taken (returnPrevious col1, row1 + 1) blackP) && checkDiag (returnPrevious col1, row1 + 1) (whiteP, blackP) (col2, row2)
													| row2 < row1 && col2 > col1 = (row1 - 1 == row2 && returnNext col1==col2) || not (taken (returnNext col1, row1 - 1) whiteP || taken (returnNext col1, row1 - 1) blackP) && checkDiag (returnNext col1, row1 - 1) (whiteP, blackP) (col2, row2)
													| otherwise = (row1 - 1 == row2 && returnPrevious col1==col2) || not (taken (returnPrevious col1, row1 - 1) whiteP || taken (returnPrevious col1, row1 - 1) blackP) &&  checkDiag (returnPrevious col1, row1 - 1) (whiteP, blackP) (col2, row2)
	
															
	
checkRow (col1,row1) (whiteP,blackP) (col2,row2)| row2>row1 =  (row1 + 1 == row2) || not (taken (col1,row1+1) whiteP || taken (col1,row1+1) blackP) && checkRow (col1,row1+1) (whiteP,blackP) (col2,row2)
												| otherwise = (row1 - 1 == row2) || not (taken (col1,row1-1) whiteP || taken (col1,row1-1) blackP) && checkRow (col1,row1-1) (whiteP,blackP) (col2,row2)
													

checkCol (col1,row1) (whiteP,blackP) (col2,row2)| col2>col1 = (returnNext col1 == col2) || not (taken (returnNext col1,row1) whiteP || taken (returnNext col1,row1) blackP) && checkCol (returnNext col1,row1) (whiteP,blackP) (col2,row2)
												| otherwise = (returnPrevious col1 == col2) || not (taken (returnPrevious col1,row1) whiteP || taken (returnPrevious col1,row1) blackP) && checkCol (returnPrevious col1,row1) (whiteP,blackP) (col2,row2)  	
													
																
	


oppositeTurn:: Player -> Player
oppositeTurn turn	| turn == White = Black
					| otherwise = White
					
{-second cond: checks if there is that piece on the board to be moved-}
isLegal:: Piece -> Board -> Location -> Bool

isLegal ( N (col1,row1) ) (turn,whiteP,blackP) (col2,row2)  | outOfBounds (col2,row2) = False
															| not ((checkSame ( N (col1,row1) ) whiteP) || (checkSame ( N (col1,row1) ) blackP)) = False
															| ((checkSame ( N (col1,row1) ) whiteP) && friendly col2 row2 White whiteP blackP) || ((checkSame ( N (col1,row1) ) blackP) && friendly col2 row2 Black whiteP blackP) = False
															| ((abs (charDiff col1 col2)) == 2) && ((abs (row2 - row1)) == 1) && not (friendly col2 row2 turn whiteP blackP) = True
															| ((abs (charDiff col1 col2)) == 1) && ((abs (row2 - row1)) == 2) && not (friendly col2 row2 turn whiteP blackP) = True
															| otherwise = False
										
isLegal ( B (col1,row1) ) (turn,whiteP,blackP) (col2,row2) 	| outOfBounds (col2,row2) = False
															| not ((checkSame ( B (col1,row1) ) whiteP) || (checkSame ( B (col1,row1) ) blackP)) = False
															| ((checkSame ( B (col1,row1) ) whiteP) && friendly col2 row2 White whiteP blackP) || ((checkSame ( B (col1,row1) ) blackP) && friendly col2 row2 Black whiteP blackP) = False
															| not(checkPath (col1,row1) (whiteP,blackP) (col2,row2)) = False
															| (abs( charDiff col1 col2 ) == abs( row2 - row1 )) && not (friendly col2 row2 turn whiteP blackP) = True
															| otherwise = False

isLegal ( Q (col1,row1) ) (turn,whiteP,blackP) (col2,row2) 	| outOfBounds (col2,row2) = False
															| not ((checkSame ( Q (col1,row1) ) whiteP) || (checkSame ( Q (col1,row1) ) blackP)) = False															
															| ((checkSame ( Q (col1,row1) ) whiteP) && friendly col2 row2 White whiteP blackP) || ((checkSame ( Q (col1,row1) ) blackP) && friendly col2 row2 Black whiteP blackP) = False
															| not(checkPath (col1,row1) (whiteP,blackP) (col2,row2)) = False
															| (row1 == row2 && not (col1 == col2)) || (not (row1 == row2) && col1 == col2) || abs( charDiff col1 col2 ) == abs( row2 - row1 ) = True
															| otherwise = False
										
isLegal ( R (col1,row1) ) (turn,whiteP,blackP) (col2,row2) 	| outOfBounds (col2,row2) = False
															| not ((checkSame ( R (col1,row1) ) whiteP) || (checkSame ( R (col1,row1) ) blackP)) = False
															| ((checkSame ( R (col1,row1) ) whiteP) && friendly col2 row2 White whiteP blackP) || ((checkSame ( R (col1,row1) ) blackP) && friendly col2 row2 Black whiteP blackP) = False
															| not(checkPath (col1,row1) (whiteP,blackP) (col2,row2)) = False
															| (row1 == row2 && not (col1 == col2)) || (not (row1 == row2) && col1 == col2) && not (friendly col2 row2 turn whiteP blackP) = True
															| otherwise = False
										
isLegal ( K (col1,row1) ) (turn,whiteP,blackP) (col2,row2) 	| outOfBounds (col2,row2) = False
															| not ((checkSame ( K (col1,row1) ) whiteP) || (checkSame ( K (col1,row1) ) blackP)) = False															
															| ((checkSame ( K (col1,row1) ) whiteP) && friendly col2 row2 White whiteP blackP) || ((checkSame ( K (col1,row1) ) blackP) && friendly col2 row2 Black whiteP blackP) = False
															| abs (row1 - row2) < 2 && abs ( charDiff col1 col2 ) < 2 && not (friendly col2 row2 turn whiteP blackP) = True
															| otherwise = False

isLegal ( P (col1,row1) ) (turn,whiteP,blackP) (col2,row2)  | outOfBounds (col2,row2) = False 
															| not ((checkSame ( P (col1,row1) ) whiteP) || (checkSame ( P (col1,row1) ) blackP)) = False															
															| ((checkSame ( P (col1,row1) ) whiteP) && friendly col2 row2 White whiteP blackP) || ((checkSame ( P (col1,row1) ) blackP) && friendly col2 row2 Black whiteP blackP) = False
{-White piece-}												| (checkSame ( P (col1,row1) ) whiteP) && row1 == 2 && row2 == 4 && col1==col2 && not (taken (col2,3) (whiteP)) && not (taken (col2,3) (blackP)) && not (taken (col2,4) (whiteP)) && not (taken (col2,4) (blackP))= True
{-Black piece-} 											| (checkSame ( P (col1,row1) ) blackP) && row1 == 7 && row2 == 5 && col1==col2 && not (taken (col2,6) (whiteP)) && not (taken (col2,6) (blackP)) && not (taken (col2,5) (whiteP)) && not (taken (col2,5) (blackP)) = True
															| (checkSame ( P (col1,row1) ) whiteP) && ((row2 - row1) == 1) && col1==col2 && not (taken (col2,row2) (whiteP)) && not (taken (col2,row2) (blackP)) = True
															| (checkSame ( P (col1,row1) ) blackP) && ((row1 - row2) == 1) && col1==col2 && not (taken (col2,row2) (whiteP)) && not (taken (col2,row2) (blackP))= True
															| (checkSame ( P (col1,row1) ) whiteP) && ((row2 - row1) == 1) && abs( charDiff col1 col2 )==1 && (taken (col2,row2) (blackP))= True
															| (checkSame ( P (col1,row1) ) blackP) && ((row1 - row2) == 1) && abs( charDiff col1 col2 )==1 && (taken (col2,row2) (whiteP))= True
															| otherwise = False
															
outOfBounds (c,r) = r>8||r<1||c>'h'||c<'a'

{-adam done-}
charDiff c1 c2 =  (ord c1) - (ord c2)



suggestMove:: Piece -> Board -> [Location]



suggestMove (R (col,row)) (p,wp,bp) = (suggestMoveRook (col,row) (col,row) (p,wp,bp) UP) ++ (suggestMoveRook (col,row) (col,row) (p,wp,bp) DOWN ) ++ (suggestMoveRook (col,row) (col,row) (p,wp,bp) LEFT ) ++ (suggestMoveRook (col,row) (col,row) (p,wp,bp) RIGHT )

suggestMove (B (col,row)) (p,wp,bp) = (suggestMoveBishop (col,row) (col,row) (p,wp,bp) DIAGONAL_UP_RIGHT ) ++ (suggestMoveBishop (col,row) (col,row) (p,wp,bp) DIAGONAL_UP_LEFT ) ++ (suggestMoveBishop (col,row) (col,row) (p,wp,bp) DIAGONAL_DOWN_RIGHT ) ++ (suggestMoveBishop (col,row) (col,row) (p,wp,bp) DIAGONAL_DOWN_LEFT )

suggestMove (Q (col,row)) (p,wp,bp) = (suggestMoveQueen (col,row) (col,row) (p,wp,bp) UP) ++ (suggestMoveQueen (col,row) (col,row) (p,wp,bp) DOWN ) ++ (suggestMoveQueen (col,row) (col,row) (p,wp,bp) LEFT ) ++ (suggestMoveQueen (col,row) (col,row) (p,wp,bp) RIGHT ) ++ (suggestMoveQueen (col,row) (col,row) (p,wp,bp) DIAGONAL_UP_RIGHT ) ++ (suggestMoveQueen (col,row) (col,row) (p,wp,bp) DIAGONAL_UP_LEFT ) ++ (suggestMoveQueen (col,row) (col,row) (p,wp,bp) DIAGONAL_DOWN_RIGHT ) ++ (suggestMoveQueen (col,row) (col,row) (p,wp,bp) DIAGONAL_DOWN_LEFT )


suggestMove (P (col,row)) b = ((suggestMovePawn (col,row) (col,row + 1) b ) ++ (suggestMovePawn (col,row) (col,row + 2) b ) ++ (suggestMovePawn (col,row) (col,row - 1) b ) ++  (suggestMovePawn (col,row) (col,row - 2) b ) ++ (suggestMovePawn (col,row) (returnNext col,row + 1) b) ++ (suggestMovePawn (col,row) (returnNext col,row - 1) b) ++ (suggestMovePawn (col,row) (returnPrevious col,row + 1) b) ++ (suggestMovePawn (col,row) (returnPrevious col,row - 1) b))

suggestMove (N (col,row)) b = (suggestMoveKnight (col,row) (returnNext (returnNext col),row + 1) b ) ++ (suggestMoveKnight (col,row) (returnNext col,row + 2) b ) ++ (suggestMoveKnight (col,row) (returnPrevious col,row + 2) b ) ++ (suggestMoveKnight (col,row) (returnPrevious (returnPrevious col),row + 1) b ) ++  (suggestMoveKnight (col,row) (returnPrevious (returnPrevious col),row - 1) b ) ++ (suggestMoveKnight (col,row) (returnPrevious col,row - 2) b ) ++ (suggestMoveKnight (col,row) (returnNext col,row - 2) b ) ++ (suggestMoveKnight (col,row) (returnNext (returnNext col),row - 1) b )


suggestMove (K (col,row)) b = (suggestMoveKing (col,row) (returnNext col,row) b) ++ (suggestMoveKing (col,row) (returnNext col,row + 1) b) ++ (suggestMoveKing (col,row) (col,row + 1) b) ++ (suggestMoveKing (col,row) (returnPrevious col,row + 1) b) ++ (suggestMoveKing (col,row) (returnPrevious col,row) b) ++ (suggestMoveKing (col,row) (returnPrevious col,row - 1) b) ++ (suggestMoveKing (col,row) (col,row - 1) b) ++ (suggestMoveKing (col,row) (returnNext col,row - 1) b)




suggestMoveRook (col1,row1) (col2,row2) b UP | row2 > 8 = []
                                             | isLegal (R (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveRook (col1,row1) (col2,row2 + 1) b UP)
                                             | otherwise = suggestMoveRook (col1,row1) (col2,row2 + 1) b UP

suggestMoveRook (col1,row1) (col2,row2) b DOWN | row2 < 1 = []
                                               | isLegal (R (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveRook (col1,row1) (col2,row2 - 1) b DOWN)
                                               | otherwise = suggestMoveRook (col1,row1) (col2,row2 - 1) b DOWN

suggestMoveRook (col1,row1) (col2,row2) b RIGHT | col2 > 'h' = []
                                                | isLegal (R (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveRook (col1,row1) (returnNext col2,row2) b RIGHT)
                                                | otherwise = suggestMoveRook (col1,row1) (returnNext col2,row2) b RIGHT    

suggestMoveRook (col1,row1) (col2,row2) b LEFT | col2 < 'a' = []
                                               | isLegal (R (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveRook (col1,row1) (returnPrevious col2,row2) b LEFT)
                                               | otherwise = suggestMoveRook (col1,row1) (returnPrevious col2,row2) b LEFT     


suggestMovePawn (col1,row1) (col2,row2) b | row2 < 1 || row2 > 8 || col2 > 'h' || col2 < 'a' = []
                                          | isLegal (P (col1,row1) ) b (col2,row2) =  [(col2,row2)]
                                          | otherwise = []


suggestMoveQueen  (col1,row1) (col2,row2) b UP | row2 > 8 = []
                                               | isLegal (Q (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveQueen (col1,row1) (col2,row2 + 1) b UP)
                                               | otherwise = suggestMoveQueen (col1,row1) (col2,row2 + 1) b UP

suggestMoveQueen (col1,row1) (col2,row2) b DOWN | row2 < 1 = []
                                                | isLegal (Q (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveQueen (col1,row1) (col2,row2 - 1) b DOWN)
                                                | otherwise = suggestMoveQueen (col1,row1) (col2,row2 - 1) b DOWN

suggestMoveQueen (col1,row1) (col2,row2) b RIGHT | col2 > 'h' = []
                                                 | isLegal (Q (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveQueen (col1,row1) (returnNext col2,row2) b RIGHT)
                                                 | otherwise = suggestMoveQueen (col1,row1) (returnNext col2,row2) b RIGHT



suggestMoveQueen (col1,row1) (col2,row2) b LEFT | col2 < 'a' = []
                                                | isLegal (Q (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveQueen (col1,row1) (returnPrevious col2,row2) b LEFT)
                                                | otherwise = suggestMoveQueen (col1,row1) (returnPrevious col2,row2) b LEFT


suggestMoveQueen (col1,row1) (col2,row2) b DIAGONAL_UP_RIGHT  | col2 > 'h' || row2 > 8 = []
                                                              | isLegal (Q (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveQueen (col1,row1) (returnNext col2,row2 + 1) b DIAGONAL_UP_RIGHT)
                                                              | otherwise = suggestMoveQueen (col1,row1) (returnNext col2,row2 + 1) b DIAGONAL_UP_RIGHT



suggestMoveQueen (col1,row1) (col2,row2) b DIAGONAL_UP_LEFT  | col2 < 'a' || row2 > 8 = []
                                                             | isLegal (Q (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveQueen (col1,row1) (returnPrevious col2,row2 + 1) b DIAGONAL_UP_LEFT)
                                                             | otherwise = suggestMoveQueen (col1,row1) (returnPrevious col2,row2 + 1) b DIAGONAL_UP_LEFT


suggestMoveQueen (col1,row1) (col2,row2) b DIAGONAL_DOWN_RIGHT  | col2 > 'h' || row2 < 1 = []
                                                                | isLegal (Q (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveQueen (col1,row1) (returnNext col2,row2 - 1) b DIAGONAL_DOWN_RIGHT)
                                                                | otherwise = suggestMoveQueen (col1,row1) (returnNext col2,row2 - 1) b DIAGONAL_DOWN_RIGHT

suggestMoveQueen (col1,row1) (col2,row2) b DIAGONAL_DOWN_LEFT  | col2 < 'a' || row2 < 1 = []
                                                               | isLegal (Q (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveQueen (col1,row1) (returnPrevious col2,row2 - 1) b DIAGONAL_DOWN_LEFT)
                                                               | otherwise = suggestMoveQueen (col1,row1) (returnPrevious col2,row2 - 1) b DIAGONAL_DOWN_LEFT


suggestMoveKnight (col1,row1) (col2,row2) b | row2 < 1 || row2 > 8 || col2 > 'h' || col2 < 'a' = []
                                            | isLegal (N (col1,row1) ) b (col2,row2) =  [(col2,row2)]
                                            | otherwise = []


suggestMoveKing (col1,row1) (col2,row2) b | row2 < 1 || row2 > 8 || col2 > 'h' || col2 < 'a' = []
                                          | isLegal (K (col1,row1) ) b (col2,row2) =  [(col2,row2)]
                                          | otherwise = [] 






suggestMoveBishop (col1,row1) (col2,row2) b DIAGONAL_UP_RIGHT | col2 > 'h' || row2 > 8 = []
                                                              | isLegal (B (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveBishop (col1,row1) (returnNext col2,row2 + 1) b DIAGONAL_UP_RIGHT)
                                                              | otherwise = suggestMoveBishop (col1,row1) (returnNext col2,row2 + 1) b DIAGONAL_UP_RIGHT



suggestMoveBishop (col1,row1) (col2,row2) b DIAGONAL_UP_LEFT | col2 < 'a' || row2 > 8 = []
                                                             | isLegal (B (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveBishop (col1,row1) (returnPrevious col2,row2 + 1) b DIAGONAL_UP_LEFT)
                                                             | otherwise = suggestMoveBishop (col1,row1) (returnPrevious col2,row2 + 1) b DIAGONAL_UP_LEFT


suggestMoveBishop (col1,row1) (col2,row2) b DIAGONAL_DOWN_RIGHT | col2 > 'h' || row2 < 1 = []
                                                                | isLegal (B (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveBishop (col1,row1) (returnNext col2,row2 - 1) b DIAGONAL_DOWN_RIGHT)
                                                                | otherwise = suggestMoveBishop (col1,row1) (returnNext col2,row2 - 1) b DIAGONAL_DOWN_RIGHT

suggestMoveBishop (col1,row1) (col2,row2) b DIAGONAL_DOWN_LEFT | col2 < 'a' || row2 < 1 = []
                                                               | isLegal (B (col1,row1) ) b (col2,row2) = ((col2,row2) : suggestMoveBishop (col1,row1) (returnPrevious col2,row2 - 1) b DIAGONAL_DOWN_LEFT)
                                                               | otherwise = suggestMoveBishop (col1,row1) (returnPrevious col2,row2 - 1) b DIAGONAL_DOWN_LEFT

                          

move:: Piece -> Location -> Board -> Board


move (P (c1,i1)) (c2,i2) (p,wp,bp) 
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | not (isLegal (P (c1,i1)) (p,wp,bp) (c2,i2)) = error ("Illegal move for piece " ++ (show (P (c1,i1))))
                                   | p == White = (Black, (replace (P (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (P (c1,i1)) (c2,i2) bp))

move (N (c1,i1)) (c2,i2) (p,wp,bp) 
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | not (isLegal (N (c1,i1)) (p,wp,bp) (c2,i2)) = error ("Illegal move for piece " ++ (show (N (c1,i1))))
                                   | p == White = (Black, (replace (N (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (N (c1,i1)) (c2,i2) bp))


move (K (c1,i1)) (c2,i2) (p,wp,bp) 
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | not (isLegal (K (c1,i1)) (p,wp,bp) (c2,i2)) = error ("Illegal move for piece " ++ (show (K (c1,i1))))
                                   | p == White = (Black, (replace (K (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (K (c1,i1)) (c2,i2) bp))


move (Q (c1,i1)) (c2,i2) (p,wp,bp) 
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | (isLegal (Q (c1,i1)) (p,wp,bp) (c2,i2)) = error ("Illegal move for piece " ++ (show (Q (c1,i1))))
                                   | p == White = (Black, (replace (Q (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (Q (c1,i1)) (c2,i2) bp))


move (R (c1,i1)) (c2,i2) (p,wp,bp) 
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | not (isLegal (R (c1,i1)) (p,wp,bp) (c2,i2)) = error ("Illegal move for piece " ++ (show (R (c1,i1))))
                                   | p == White = (Black, (replace (R (c1,i1)) (c2,i2) wp) ,bp)
                                   | otherwise = (White,wp,(replace (R (c1,i1)) (c2,i2) bp))

move (B (c1,i1)) (c2,i2) (p,wp,bp) 
                                   | taken (c1,i1) bp && p == White = error "This is White player's turn, Black can't move."
                                   | taken (c1,i1) wp && p == Black = error "This is Black player's turn, White can't move."
                                   | not (isLegal (B (c1,i1)) (p,wp,bp) (c2,i2)) = error ("Illegal move for piece " ++ (show (B (c1,i1))))
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
