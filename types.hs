module Types where


data Piece = Pawn Bool | Knight Bool | Bishop Bool 
           | Rook Bool | Queen Bool  | King Bool 
            deriving (Show, Eq, Ord)

data SpecialMove = Enp | CastleLong | CastleShort | Promote deriving Eq

type Move  = (Position, Position, Maybe SpecialMove)
type Position = (Int, Int)




-- "global variables record"
data State = State { turn         :: Bool,
                     history      :: [Move],
                     legalmoves   :: [Move],
                     whiteattacks :: [Position],
                     blackattacks :: [Position],
                     whitepieces  :: [(Piece, Position)],
                     blackpieces  :: [(Piece, Position)],
                     empty :: [Position],
                     wl :: Bool,
                     ws :: Bool,
                     bl :: Bool,
                     bs :: Bool
                    }

start = State { turn = True, history = [], legalmoves = [], whiteattacks = [], blackattacks = [],
                wl = True, ws = True, bl = True, bs = True,
                whitepieces =   [ (Pawn True, (6, 0)), (Pawn True, (6, 1)), (Pawn True, (6, 2)), (Pawn True, (6, 3))
                                , (Pawn True, (6, 4)), (Pawn True, (6, 5)), (Pawn True, (6, 6)), (Pawn True, (6, 7))
                                , (Rook True, (7, 0)), (Knight True, (7, 1)), (Bishop True, (7, 2)), (Queen True, (7, 3))
                                , (King True, (7, 4)), (Bishop True, (7, 5)), (Knight True, (7, 6)), (Rook True, (7, 7))
                                ],
                blackpieces =   [ (Rook False, (0, 0)), (Knight False, (0, 1)), (Bishop False, (0, 2)), (Queen False, (0, 3))
                                , (King False, (0, 4)), (Bishop False, (0, 5)), (Knight False, (0, 6)), (Rook False, (0, 7))
                                , (Pawn False, (1, 0)), (Pawn False, (1, 1)), (Pawn False, (1, 2)), (Pawn False, (1, 3))
                                , (Pawn False, (1, 4)), (Pawn False, (1, 5)), (Pawn False, (1, 6)), (Pawn False, (1, 7))
                                ],
                empty = [(2,0), (2,1), (2,2), (2,3), (2,4), (2,5), (2,6), (2,7),
                         (3,0), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7),
                         (4,0), (4,1), (4,2), (4,3), (4,4), (4,5), (4,6), (4,7),
                         (5,0), (5,1), (5,2), (5,3), (5,4), (5,5), (5,6), (5,7)]
}

random = State { turn = True, history = [], legalmoves = [], whiteattacks = [], blackattacks = [],
        wl = False, ws = False, bl = False, bs = False,
        whitepieces = [(Pawn True, (1, 1)), (Pawn True, (3, 3)), (Rook True, (7, 5)), (Knight True, (2, 1)), (Bishop True, (6, 7)), (Queen True, (0, 3)), (King True, (5, 4)), (Bishop True, (4, 0)), (Knight True, (6, 2)), (Rook True, (0, 2))],
        blackpieces = [(Pawn False, (2, 6)), (Pawn False, (7, 3)), (Rook False, (3, 7)), (Knight False, (1, 7)), (Bishop False, (7, 0)), (Queen False, (6, 5)), (King False, (0, 5)), (Bishop False, (5, 6)), (Knight False, (6, 4)), (Rook False, (4, 4))],
        empty = [(0, 0), (0, 1), (0, 4), (0, 6), (0, 7), (1, 0), (1, 2), (1, 3), (1, 4), (1, 5), (1, 6), (1, 7), (2, 0), (2, 2), (2, 3), (2, 4), (2, 5), (2, 7), (3, 0), (3, 1), (3, 2), (3, 4), (3, 5), (3, 6), (4, 1), (4, 2), (4, 3), (4, 5), (4, 6), (4, 7), 
        (5, 0), (5, 1), (5, 2), (5, 3), (5, 5), (5, 7), (6, 0), (6, 1), (6, 3), (6, 6), (7, 1), (7, 2), (7, 4), (7, 5), (7, 6), (7, 7)]

      }

nopieces = State {  turn = True, history = [], legalmoves = [], whiteattacks = [], blackattacks = [], whitepieces = [], blackpieces = [],
                    wl = True, ws = True, bl = True, bs = True,
                    empty =    [(0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (0,6), (1,7),
                                (1,0), (1,1), (1,2), (1,3), (1,4), (1,5), (1,6), (1,7),
                                (2,0), (2,1), (2,2), (2,3), (2,4), (2,5), (2,6), (2,7),
                                (3,0), (3,1), (3,2), (3,3), (3,4), (3,5), (3,6), (3,7),
                                (4,0), (4,1), (4,2), (4,3), (4,4), (4,5), (4,6), (4,7),
                                (5,0), (5,1), (5,2), (5,3), (5,4), (5,5), (5,6), (5,7),
                                (6,0), (6,1), (6,2), (6,3), (6,4), (6,5), (6,6), (6,7),
                                (7,0), (7,1), (7,2), (7,3), (7,4), (7,5), (7,6), (7,7)]

}