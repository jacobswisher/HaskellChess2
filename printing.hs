module Printing where


-- import Data.Foldable (toList)
-- import Text.Printf
-- import Data.Char
import Data.List(find, elemIndex)
import Data.Maybe(isJust)
import Types



showspot :: Piece -> Char
showspot (Pawn True)    = 'P'
showspot (Knight True)  = 'N'
showspot (Bishop True)  = 'B'
showspot (Rook True)    = 'R'
showspot (Queen True)   = 'Q'
showspot (King True)    = 'K'
showspot (Pawn False)   = 'p'
showspot (Knight False) = 'n'
showspot (Bishop False) = 'b'
showspot (Rook False)   = 'r'
showspot (Queen False)  = 'q'
showspot (King False)   = 'k'


-- abstractspot :: Char -> Maybe Piece
-- abstractspot 'P' = Just $ Pawn True
-- abstractspot 'N' = Just $ Knight True
-- abstractspot 'B' = Just $ Bishop True
-- abstractspot 'R' = Just $ Rook True
-- abstractspot 'Q' = Just $ Queen True
-- abstractspot 'K' = Just $ King True
-- abstractspot 'p' = Just $ Pawn False
-- abstractspot 'n' = Just $ Knight False
-- abstractspot 'b' = Just $ Bishop False
-- abstractspot 'r' = Just $ Rook Flase
-- abstractspot 'q' = Just $ Queen False
-- abstractspot 'k' = Just $ King False
-- abstractspot  _  = Nothing

extendfen :: String -> String
extendfen ('8':xs) = "--------" ++ extendfen xs
extendfen ('7':xs) = "-------" ++ extendfen xs
extendfen ('6':xs) = "------" ++ extendfen xs
extendfen ('5':xs) = "-----" ++ extendfen xs
extendfen ('4':xs) = "----" ++ extendfen xs
extendfen ('3':xs) = "---" ++ extendfen xs
extendfen ('2':xs) = "--" ++ extendfen xs
extendfen ('1':xs) = "-" ++ extendfen xs
extendfen ('/':xs) = extendfen xs
extendfen (x:xs)   = [x] ++ extendfen xs
extendfen s = s

-- getboard :: String -> Board
-- getboard string = fromList $ map abstractspot string

-- showboard :: Board -> String
-- showboard b = map showspot (toList b)

statetostring :: State -> String
statetostring s@(State{blackpieces, whitepieces, empty}) =
  let func cur@(row,col)  | row > 7 = ""
                          | col > 7 = '\n':func (row+1,0)
                          | cur `elem` empty = '-':func (row,col+1)
                          | isJust $ find (\(_, y) -> y == cur) blackpieces = showspot (fst (makejust (find (\(_, y) -> y == cur) blackpieces))):func (row,col+1)
                          | isJust $ find (\(_, y) -> y == cur) whitepieces = showspot (fst (makejust (find (\(_, y) -> y == cur) whitepieces))):func (row,col+1)
                          | otherwise = error ("square is not empty but has no piece")
  in (if turn s then "Whites Turn\n" else "Blacks Turn") ++ func (0,0)


makejust :: Maybe a -> a 
makejust Nothing = error ("why")
makejust (Just a) = a

-- might be unnecessary
deletenewline :: String -> String
deletenewline [] = []
deletenewline ('\n':xs) = xs
deletenewline (x:xs) = x:deletenewline xs

-- first char is turn
stringtostate :: String -> State
stringtostate s = 
  let postextension acc [] _ = acc
      postextension acc s (r,c) | c > 7  = postextension acc s  (r+1, 0)
      postextension acc ('-':cs) (r,c)   = postextension acc cs (r, c+1)
      postextension acc ('P':cs) d@(r,c) = postextension (removeempty (acc{whitepieces = (Pawn True,    d):whitepieces acc}) d) cs (r,c+1)
      postextension acc ('p':cs) d@(r,c) = postextension (removeempty (acc{blackpieces = (Pawn False,   d):blackpieces acc}) d) cs (r,c+1)
      postextension acc ('N':cs) d@(r,c) = postextension (removeempty (acc{whitepieces = (Knight True,  d):whitepieces acc}) d) cs (r,c+1)
      postextension acc ('n':cs) d@(r,c) = postextension (removeempty (acc{blackpieces = (Knight False, d):blackpieces acc}) d) cs (r,c+1)
      postextension acc ('B':cs) d@(r,c) = postextension (removeempty (acc{whitepieces = (Bishop True,  d):whitepieces acc}) d) cs (r,c+1)
      postextension acc ('b':cs) d@(r,c) = postextension (removeempty (acc{blackpieces = (Bishop False, d):blackpieces acc}) d) cs (r,c+1)
      postextension acc ('R':cs) d@(r,c) = postextension (removeempty (acc{whitepieces = (Rook True,    d):whitepieces acc}) d) cs (r,c+1)
      postextension acc ('r':cs) d@(r,c) = postextension (removeempty (acc{blackpieces = (Rook False,   d):blackpieces acc}) d) cs (r,c+1)
      postextension acc ('Q':cs) d@(r,c) = postextension (removeempty (acc{whitepieces = (Queen True,   d):whitepieces acc}) d) cs (r,c+1)
      postextension acc ('q':cs) d@(r,c) = postextension (removeempty (acc{blackpieces = (Queen False,  d):blackpieces acc}) d) cs (r,c+1)
      postextension acc ('K':cs) d@(r,c) = postextension (removeempty (acc{whitepieces = (King True,    d):whitepieces acc}) d) cs (r,c+1)
      postextension acc ('k':cs) d@(r,c) = postextension (removeempty (acc{blackpieces = (King False,   d):blackpieces acc}) d) cs (r,c+1)
      removeempty s@(State{empty = e}) (r,c) = s{empty = (\(x,_:y)->x++y) $ splitAt (makejust $ elemIndex (r,c) e) e}
  in postextension nopieces (extendfen s) (0,0)

-- boardtostring :: Board -> String
-- boardtostring b = "\n" ++ "==ABCDEFGH==" ++ "\n" ++ (unlines $ zipWith (++)(zipWith (++) ["8 ","7 ","6 ","5 ","4 ","3 ","2 ","1 "] (format $ (showboard b))) [" 8"," 7"," 6"," 5"," 4"," 3"," 2"," 1"] ) ++ "==ABCDEFGH==" ++ "\n"


-- --input will look like "a2c6"
-- stringtomove :: String -> Move
-- stringtomove s | length s /= 4 = error ("error in stm: " ++ s)
-- stringtomove s = (ctr (s!!1) * 8 + ctc (s!!0), ctr (s!!3) * 8 + ctc (s!!2))

-- -- printboard :: BitBoard -> IO()
-- -- printboard bb = putStr(boardtostring $ fromList $ bittoboard bb)

printstate :: State -> IO()
printstate s = putStr $ show s

instance Show State where
  show s = statetostring s

-- printmove :: Move -> String
-- printmove (o,d) = (printpos o) ++ " to " ++ (printpos d)

-- printmoves :: [Move] -> [String]
-- printmoves ms = map printmove ms