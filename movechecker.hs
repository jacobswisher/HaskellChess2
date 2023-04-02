module Movechecker where

--Necessary imports
import Types
import Chess
import Printing
import Data.Maybe(isNothing, isJust)

--FOR TESTING ONLY
-- import Boards
-- import Printing


-- Returns a list of up to four potentially legal pawn moves
getpawnmoves :: State -> Position -> [Move]
getpawnmoves s@(State{turn = False}) p@(r,c) =
  let m1 = if r == 1 && isempty s (r+2,c) then [(p,(r+2,c))] else []
      m2 = if isempty s (r+1,c) then (p,(r+1,c)):m1 else []
      m3 = if c /= 0 && (isenemy s (r+1,c-1)) then (p,(r+1,c-1)):m2 else m2
      m4 = if c /= 7 && (isenemy s (r+1,c+1)) then (p,(r+1,c+1)):m3 else m3
  in m4
getpawnmoves s@(State{turn = True}) p@(r,c) =
  let m1 = if r == 6 && isempty s (r-2,c) then [(p,(r-2,c))] else []
      m2 = if isempty s (r-1,c) then (p,(r-1,c)):m1 else []
      m3 = if c /= 7 && (isenemy s (r-1,c+1)) then (p,(r-1,c+1)):m2 else m2
      m4 = if c /= 0 && (isenemy s (r-1,c-1)) then (p,(r-1,c-1)):m3 else m3
  in m4


--returns a list of potentially legal rook moves
getrookmoves :: State -> Position -> [Move]
getrookmoves s (orow,ocol)  =
  let up (r,c)    | r < 0         = []
                  | isempty s (r,c) = (r,c):up (r-1,c)
                  | isenemy s (r,c) = [(r,c)]
                  | otherwise     = []
      down (r,c)  | r > 7         = []
                  | isempty s (r,c) = (r,c):down (r+1,c)
                  | isenemy s (r,c) = [(r,c)]
                  | otherwise     = []
      left (r,c)  | c < 0         = []
                  | isempty s (r,c) = (r,c):left (r,c-1)
                  | isenemy s (r,c) = [(r,c)]
                  | otherwise     = []
      right (r,c) | c > 7         = []
                  | isempty s (r,c) = (r,c):right (r,c+1)
                  | isenemy s (r,c) = [(r,c)]
                  | otherwise     = []
  in zip (repeat (orow,ocol)) $ up (orow-1,ocol) ++ down (orow+1,ocol) ++ left (orow,ocol-1) ++ right (orow,ocol+1)

getbishopmoves :: State -> Position -> [Move]
getbishopmoves s (orow,ocol)  =
  let upleft (r,c)    | r < 0 || c < 0 = []
                      | isempty s (r,c)  = (r,c):upleft (r-1,c-1)
                      | isenemy s (r,c)  = [(r,c)]
                      | otherwise      = []
      upright (r,c)   | r < 0 || c > 7 = []
                      | isempty s (r,c)  = (r,c):upright (r-1,c+1)
                      | isenemy s (r,c)  = [(r,c)]
                      | otherwise      = []
      downleft (r,c)  | r > 7 || c < 0 = []
                      | isempty s (r,c)  = (r,c):downleft (r+1,c-1)
                      | isenemy s (r,c)  = [(r,c)]
                      | otherwise      = []
      downright (r,c) | r > 7 || c > 7 = []
                      | isempty s (r,c)  = (r,c):downright (r+1,c+1)
                      | isenemy s (r,c)  = [(r,c)]
                      | otherwise      = []
  in zip (repeat (orow,ocol)) $ (upleft (orow-1,ocol-1) ++ upright (orow-1,ocol+1) ++ downleft (orow+1,ocol-1) ++ downright (orow+1,ocol+1))


--gets a list of potentially legal knight moves
getknightmoves :: State -> Position -> [Move]
getknightmoves s p@(r,c) =
  let m1 = if r > 1 && c > 0 && notfriendlyfire s (r-2,c+1) then [(p,(r-2,c+1))]  else []
      m2 = if r > 1 && c < 7 && notfriendlyfire s (r-2,c-1) then (p,(r-2,c-1)):m1 else m1
      m3 = if r < 6 && c > 0 && notfriendlyfire s (r+2,c+1) then (p,(r+2,c+1)):m2 else m2
      m4 = if r < 6 && c < 7 && notfriendlyfire s (r+2,c-1) then (p,(r+2,c-1)):m3 else m3
      m5 = if r > 0 && c < 6 && notfriendlyfire s (r-1,c-2) then (p,(r-1,c-2)):m4 else m4
      m6 = if r < 7 && c < 6 && notfriendlyfire s (r+1,c-2) then (p,(r+1,c-2)):m5 else m5
      m7 = if r > 0 && c > 1 && notfriendlyfire s (r-1,c+2) then (p,(r-1,c+2)):m6 else m6
      m8 = if r < 7 && c > 1 && notfriendlyfire s (r+1,c+2) then (p,(r+1,c+2)):m7 else m7
  in m8

--gets a list of all potentially valid king moves excluding castling
getkingmoves :: State -> Position -> [Move]
getkingmoves s p@(r,c) =
  let m1 = if c > 0 && notfriendlyfire s (r,c-1)            then [(p,(r,c-1))]  else []
      m2 = if r > 0 && notfriendlyfire s (r-1,c)            then (p,(r-1,c)):m1 else m1
      m3 = if c < 7 && notfriendlyfire s (r,c+1)            then (p,(r,c+1)):m2 else m2
      m4 = if r < 7 && notfriendlyfire s (r+1,c)            then (p,(r+1,c)):m3 else m3
      m5 = if r > 0 && c > 0 && notfriendlyfire s (r-1,c-1) then (p,(r-1,c-1)):m4 else m4
      m6 = if r > 0 && c < 7 && notfriendlyfire s (r-1,c+1) then (p,(r-1,c+1)):m5 else m5
      m7 = if r < 7 && c > 0 && notfriendlyfire s (r+1,c-1) then (p,(r+1,c-1)):m6 else m6
      m8 = if r < 7 && c < 7 && notfriendlyfire s (r+1,c+1) then (p,(r+1,c+1)):m7 else m7
  in filter (notcheckmove s) (m8 ++ getcastlemoves s)






-- gets list of potentially valid moves by castling
getcastlemoves :: State -> [Move]
getcastlemoves s | turn s =
  let m1 = if wl s && (7,1) `elem` (empty s) && (7,2) `elem` (empty s) && (7,3) `elem` (empty s) then [((7,4),(7,2))]  else []
      m2 = if ws s && (7,5) `elem` (empty s) && (7,6) `elem` (empty s) then ((7,4),(7,6)):m1 else m1
  in m2
                 | otherwise =
  let m1 = if bl s && (0,1) `elem` (empty s) && (0,2) `elem` (empty s) && (0,3) `elem` (empty s) then [((0,4),(0,2))]  else []
      m2 = if bs s && (0,5) `elem` (empty s) && (0,6) `elem` (empty s) then ((0,4),(0,6)):m1 else m1
  in m2




--returns a list of moves for a given spot at a specified position
getmoves :: State -> [Move]
getmoves s@(State {whitepieces, turn = True})  = concatMap (getmovesforpiece s) whitepieces
getmoves s@(State {blackpieces, turn = False}) = concatMap (getmovesforpiece s) blackpieces

getmovesforpiece :: State -> (Piece, Position) -> [Move]
getmovesforpiece s (Pawn _, p)   = getpawnmoves    s p
getmovesforpiece s (Knight _, p) = getknightmoves  s p
getmovesforpiece s (Bishop _, p) = getbishopmoves  s p
getmovesforpiece s (Rook _, p)   = getrookmoves    s p
getmovesforpiece s (Queen _, p)  = getbishopmoves  s p ++ getrookmoves s p
getmovesforpiece s (King _, p)   = getkingmoves    s p


updatemoves :: State -> State
updatemoves s = updateattacks s{legalmoves = moves}
  where updateattacks state | turn state = state{whiteattacks = map snd moves}
        updateattacks state | otherwise  = state{blackattacks = map snd moves}
        moves = getmoves s

updateturn :: State -> State
updateturn s = s{turn = not $ turn s}

domove :: State -> Move -> State
domove s m | not $ m `elem` (legalmoves s) = error ("illegal move attempted:" ++ show m)
domove s@(State{whitepieces, blackpieces, empty}) (o,d) 
      | turn s && d `elem` empty = s{whitepieces = del ((piece,d):whitepieces) (piece,o), empty = del (o:empty) d}
      | turn s                   = s{whitepieces = del ((piece,d):whitepieces) (piece,o), empty = del (o:empty) d, blackpieces = del blackpieces (deadpiece,d)}
      | d `elem` empty           = s{blackpieces = del ((piece,d):blackpieces) (piece,o), empty = del (o:empty) d}
      | otherwise                = s{blackpieces = del ((piece,d):blackpieces) (piece,o), empty = del (o:empty) d, whitepieces = del whitepieces (deadpiece,d)}
  where piece = getpieceatpos (if turn s then whitepieces else blackpieces) o
        deadpiece = getpieceatpos (if turn s then blackpieces else whitepieces) d

dofullmove :: State -> Move -> State
dofullmove s m = updatemoves $ updateturn (domove s m)




getpieceatpos :: [(Piece, Position)] -> Position -> Piece
getpieceatpos [] _ = error ("piece not in piecelist")
getpieceatpos ((p,d):xs) c | d == c    = p
                           | otherwise = getpieceatpos xs c

del :: Eq a => [a] -> a -> [a]
del [] _ = error ("attempted to delete element not in list")
del (x:xs) a | x == a    = xs
             | otherwise = x:del xs a

-- illegally moving king into check
notcheckmove :: State -> Move -> Bool
notcheckmove s (o,d) = not $ d `elem` coveredbyenemy
  where coveredbyenemy = if turn s then blackattacks s else whiteattacks s

-- --returns true if and only if the current sides king can be attacked by an enemy piece
-- incheck :: State -> Bool
-- incheck s  = (king.&.enemyattacks) /= 0
--   where king = if turn s then whitekings (board s) else blackkings (board s)
--         enemyattacks = if turn s then blackattacks s else whiteattacks s

-- --returns true if the current player is in checkmate or stalemate
-- gameover :: State -> Bool
-- gameover s = (checkmate s) || (stalemate s)

-- --not in check and not able to move
-- stalemate :: State -> Bool
-- stalemate s = not (incheck s) && null (getmoves s)

-- --in check and not able to move
-- checkmate :: State -> Bool
-- checkmate s = incheck s && null (getmoves s)

-- --TODO
-- updateattacks :: State -> State
-- updateattacks s | not $ turn s = s{whiteattacks = parsemoves (getsudomoves (updateturn s)) 0, blackattacks = parsemoves (getsudomoves s) 0}
--                 | otherwise    = s{blackattacks = parsemoves (getsudomoves (updateturn s)) 0, whiteattacks = parsemoves (getsudomoves s) 0}

-- parsemoves :: [Move] -> Word64 -> Word64
-- parsemoves [] n     = n
-- parsemoves (x:xs) n = parsemoves xs (n `setBit` (fromIntegral (snd x)))

-- --this is a helper function that makes a state out of all of the specified information and stores it for function use.
-- makestate :: BitBoard -> Side -> State
-- makestate b t = updateattacks State {
--   board = b,
--   turn = t,
--   history = [],
--   whiteattacks = 0,
--   blackattacks = 0,
--   wl = True,
--   ws = True,
--   bl = True,
--   bs = True}

-- domove :: State -> Move -> State
-- domove s m = updateattacks (dmove s m)


-- domoves :: State -> State
-- domoves s = domove s $ head (getmoves s)











--comment
