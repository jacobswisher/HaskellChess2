module Chess where

--Imports
import Types
import Data.Maybe(isNothing, isJust)
-- import Boards


{-
board representation is a single vector:
8 | 0  1  2  3  4  5  6  7  | 8 0
7 | 8  9  10 11 12 13 14 15 | 7 1
6 | 16 17 18 19 20 21 22 23 | 6 2
5 | 24 25 26 27 28 29 30 31 | 5 3
4 | 32 33 34 35 36 37 38 39 | 4 4
3 | 40 41 42 43 44 45 46 47 | 3 5
2 | 48 49 50 51 52 53 54 55 | 2 6
1 | 56 57 58 59 60 61 62 63 | 1 7
    A  B  C  D  E  F  G  H
-}
-- [[0 ,1 ,2 ,3 ,4 ,5 ,6 ,7 ], 
--  [8 ,9 ,10,11,12,13,14,15],
--  [16,17,18,19,20,21,22,23],
--  [24,25,26,27,28,29,30,31],
--  [32,33,34,35,36,37,38,39],
--  [40,41,42,43,44,45,46,47],
--  [48,49,50,51,52,53,54,55],
--  [56,57,58,59,60,61,62,63]]



-- getspot :: Board -> Position -> Maybe Piece
-- getspot _ (r,c) | r < 0 || r > 7 || c < 0 || c > 7 = error ("index out of range. getspot:" ++ show (r,c))
-- getspot b (r,c) = b !! r !! c

-- setspot :: Board -> Position -> Maybe Piece -> Board
-- setspot _ (r,c) _ | r < 0 || r > 7 || c < 0 || c > 7 = error ("index out of range. setspot:" ++ show (r,c))
-- setspot b (r,c) mp = lrows ++ (first ++ mp:second):rrows
--       where (first, _:second) = splitAt c $ b !! r 
--             (lrows, _:rrows) = splitAt r b

-- rawmove :: Move -> Board -> Board
-- rawmove (o,d) b = setspot (setspot b d mp) o Nothing
--       where mp = getspot b o

notfriendlyfire :: State -> Position -> Bool
notfriendlyfire s d = isempty s d || isenemy s d


isempty :: State -> Position -> Bool
isempty (State{empty}) d = d `elem` empty

isenemy :: State -> Position -> Bool
isenemy (State{turn = t, whitepieces, blackpieces, empty}) d 
    | d `elem` empty  = False
    | t               = any (\(_, p) -> p == d) blackpieces
    | otherwise       = any (\(_, p) -> p == d) whitepieces




-- --Intermediary between states and premove
-- dmove :: State -> Move -> State
-- dmove s m = updatecastle (updateturn (updatehistory (premove s m) m)) m


-- --update board will update the state board when the board changes.
-- updateboard :: State -> BitBoard -> State
-- updateboard state bb = state {board = bb}

-- --updates the history of the current state.
-- updatehistory :: State -> Move -> State
-- updatehistory state m = state {history = (history state) ++ [m]}

-- --updates the current turn of the state.
-- updateturn :: State -> State
-- updateturn state = state {turn = not (turn state)}


-- --TODO probably a better way to do this
-- updatecastle :: State -> Move -> State
-- updatecastle state (0,d)  = state {bl = False}
-- updatecastle state (7,d)  = state {bs = False}
-- updatecastle state (4,d)  = state {bs = False, bl = False}
-- updatecastle state (56,d) = state {wl = False}
-- updatecastle state (63,d) = state {ws = False}
-- updatecastle state (60,d) = state {ws = False, wl = False}
-- updatecastle s m = s

-- empty :: Board -> Position -> Bool
-- empty b p = isNothing $ getspot b p

-- --makes sure that the piece being attacked is not the same color, if so notfriendlyfire returns True





-- -- this function will handle moving a piece after it has been cleared
-- -- this will help with castling, en passant, and promote
-- -- also takes a state object so that it can determine if castling can be done
-- -- TODO
-- premove :: State -> Move -> State
-- premove s (o,d)  | (getspot (board s) o) == WhitePawn && (row o == 1) = promote s (o,d)
--                  | (getspot (board s) o) == BlackPawn && (row o == 6) = promote s (o,d)
--                  | o == 60 && whitecastle s (o,d)                     = docastle s (o,d)
--                  | o == 4  && blackcastle s (o,d)                     = docastle s (o,d)
--                  | otherwise                                          = updateboard s (executemove (board s) (o,d))


-- --This function handles the promoting of pawns into queens, updates the board with a new queen piece.
-- promote :: State -> Move -> State
-- promote s (o,d) = updateboard s (changevariable (removepiece (removepiece (board s) d) o) (if turn s then WhiteQueen else BlackQueen) d)

-- --the driver for castling, actually performs the moves to move the pieces.
-- docastle :: State -> Move -> State
-- docastle s (60,58) = updateboard s (executemove (executemove (board s) (56,59)) (60,58))
-- docastle s (60,62) = updateboard s (executemove (executemove (board s) (63,61)) (60,62))
-- docastle s (4,6)    = updateboard s (executemove (executemove (board s) (7,5)) (4,6))
-- docastle s (4,2)    = updateboard s (executemove (executemove (board s) (0,3)) (4,2))

-- --checks for castling on white team.
-- whitecastle :: State -> Move -> Bool
-- whitecastle s (60, 62) = empty (board s) 61 && empty (board s) 62 && ws s && testBit (whiterooks (board s)) 63
-- whitecastle s (60, 58) = empty (board s) 57 && empty (board s) 58 && empty (board s) 59 && wl s && testBit (whiterooks (board s)) 56
-- whitecastle s _        = False


-- --checks for castling on black team.
-- blackcastle :: State -> Move -> Bool
-- blackcastle s (4, 6) = empty (board s) 5 && empty (board s) 6 && bs s && testBit (blackrooks (board s)) 7
-- blackcastle s (4, 2) = empty (board s) 1 && empty (board s) 2 && empty (board s) 3 && bl s && testBit (blackrooks (board s)) 0
-- blackcastle s _      = False



-- isenemy bb i True  =  testBit (blackpieces bb) (fromIntegral i)
-- isenemy bb i False =  testBit (whitepieces bb) (fromIntegral i)


-- moveisanattack :: BitBoard -> Move -> Bool
-- moveisanattack bb (o,d) = exists bb d






-- --comment
