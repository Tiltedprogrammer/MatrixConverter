module QTree where

import System.IO
import Data.List

import Prelude hiding ((<>))

import Text.PrettyPrint

-- Need a way to dump QTree representation for further to-hardware transformation, e.g. show Val a == Val a, default deriving works well
-- Need a way to convert file representation, e.g. mtx to QTree
-- Tests to verify the correctnress of multiplication

data MatrixMarket a = MatrixMarket {rows :: Int, columns :: Int, nonzeroes :: Int, entries :: [(Int, Int, a)]} deriving Show
data MatrixMarketElem a = MatrixMarketElem {row :: Int, column :: Int, value :: a}

data QTree a = QNone | QVal a | QNode (QTree a) (QTree a) (QTree a) (QTree a) | QError deriving (Eq)
data MaskQTree = MQNone | MQVal | MQNode MaskQTree MaskQTree MaskQTree MaskQTree deriving (Eq)
data QTreeCell = TL | TR | BL | BR -- top-left, top-right etc.


data TextTree a = PoitinTextTree (QTree a) | HaskellTextTree (QTree a)
data TextMask =  PoitinTextMask MaskQTree | HaskellTextMask MaskQTree

class PP a where
    pp :: a -> Doc

instance (Show a) => PP (TextTree a) where

    pp (PoitinTextTree QNone) = text "QNone"
    pp (PoitinTextTree (QVal a)) = (text "QVal") <+> lparen <> (text (show a)) <> rparen
    pp (PoitinTextTree (QError)) = text "QError"
    pp (PoitinTextTree (QNode tl tr bl br)) = (text "QNode") <+> lparen <> hcat (punctuate comma [pp (PoitinTextTree tl), pp (PoitinTextTree tr), pp  (PoitinTextTree  bl), pp (PoitinTextTree br)]) <> rparen

    pp (HaskellTextTree QNone) = text "QNone"
    pp (HaskellTextTree (QVal a)) = lparen <> (text "QVal") <+> (text (show a)) <> rparen
    pp (HaskellTextTree QError) = text "QError"
    pp (HaskellTextTree (QNode tl tr bl br)) = lparen <> (text "QNode") <+> hcat (punctuate space [pp (HaskellTextTree tl), pp (HaskellTextTree tr), pp (HaskellTextTree bl), pp (HaskellTextTree br)]) <> rparen



instance PP (TextMask) where

    
    pp (HaskellTextMask MQNone) = text "MQNone"
    pp (HaskellTextMask MQVal) = text "MQVal"
    pp (HaskellTextMask (MQNode tl tr bl br)) = lparen <> (text "MQNode") <+> hcat (punctuate space [pp (HaskellTextMask tl), pp (HaskellTextMask tr), pp (HaskellTextMask bl), pp (HaskellTextMask $ br)]) <> rparen
    
    pp (PoitinTextMask MQNone) = text "MQNone"
    pp (PoitinTextMask MQVal) = text "MQVal"
    pp (PoitinTextMask (MQNode tl tr bl br)) = (text "MQNode") <+> lparen <> hcat (punctuate comma [pp (PoitinTextMask tl), pp (PoitinTextMask tr), pp (PoitinTextMask bl), pp (PoitinTextMask br)]) <> rparen
            

instance Show (TextMask) where
    show = render . pp
instance Show a => Show (TextTree a) where
    show = render . pp

isCoordinate = elem "coordinate"
isInteger = elem "integer"
isGeneral = elem "general"

skipComments [] = []
skipComments (l:ls) = if "%" `isPrefixOf` (l) then skipComments ls else l:ls


-- read matrix market file, for now only coordinate integer is supported
readMtx :: FilePath -> IO (MatrixMarket Int)
readMtx path = do
    content <- readFile path
    let contentLines = lines content
        mtx = case contentLines of
            [] -> error "Empty file"
            (l:ls) -> if isCoordinate (words l) && isInteger (words l) && isGeneral (words l) then

                            let noComments = skipComments ls in
                                case noComments of
                                    l:l':ls -> case words l of
                                                    [rows, columns, entries] ->
                                                        MatrixMarket {rows = read rows :: Int,
                                                                      columns = read columns :: Int,
                                                                      nonzeroes = read entries :: Int,
                                                                      entries = sortBy cmp $ map f
                                                                          ([words x | x <- l':ls])
                                                                          }
                                                                          where f rce = (case rce of
                                                                                        [r, c, e] -> ((read r :: Int) - 1, (read c :: Int) - 1, read e :: Int)
                                                                                        _ -> error "can not parse, possibly due to empty lines at the bottom")
                                                                                cmp (lr,lc,_) (rr,rc,_) = compare (lr,lc) (rr,rc)
                                    _ -> error "bad format"
                            else error "Only coordinate integer general is supported"


    return mtx

-- TODO split COO matrix to QTree
-- assume MatrixMarket is sorted

cellOrientation :: (Int, Int, a) -> QTreeCell
cellOrientation (row, column, _)
  | row `mod` 2 == 0 =
       if column `mod` 2 == 0 then TL
       else TR
  | column `mod` 2 == 0 = BL
  | otherwise = BR

--size of a matrix should be power of 2 (at least formally)
matrixMarketToQTree :: MatrixMarket a -> QTree a
matrixMarketToQTree m = matrixMarketToQTreeHelper (0,0) m where

      matrixMarketToQTreeHelper topLeftCorner'@(topLeftRow, topLeftColumn)
                                mtx@MatrixMarket {rows = rows',
                                  columns = columns',
                                  nonzeroes = nonzeroes',
                                  entries = entries'} = if rows' == 2 && rows' == columns'
                                                        then
                                                            case entries' of
                                                                [(_,_,tl),(_,_,tr),(_,_,bl),(_,_,br)] -> QNode (QVal tl) (QVal tr) (QVal bl) (QVal br)
                                                                [entry1@(row1,col1,value1),entry2@(row2,col2,value2),entry3@(row3,col3,value3)] -> case cellOrientation entry1 of
                                                                    TL -> case cellOrientation entry2 of
                                                                        TR -> case cellOrientation entry3 of
                                                                            BL -> QNode (QVal value1) (QVal value2) (QVal value3) QNone
                                                                            _ ->  QNode (QVal value1) (QVal value2) QNone (QVal value3)
                                                                        BL -> case cellOrientation entry3 of
                                                                            TR -> QNode (QVal value1) (QVal value3) (QVal value2) QNone
                                                                            _  -> QNode (QVal value1) QNone (QVal value2) (QVal value3)
                                                                        _  -> case cellOrientation entry3 of
                                                                            TR -> QNode (QVal value1) (QVal value3) QNone (QVal value2)
                                                                            _  -> QNode (QVal value1) QNone (QVal value3) (QVal value2)
                                                                    TR -> case cellOrientation entry2 of
                                                                        TL -> case cellOrientation entry3 of
                                                                            BL -> QNode (QVal value2) (QVal value1) (QVal value3) QNone
                                                                            _ ->  QNode (QVal value2) (QVal value1) QNone (QVal value3)
                                                                        BL -> case cellOrientation entry3 of
                                                                            TL -> QNode (QVal value3) (QVal value1) (QVal value2) QNone
                                                                            _  -> QNode QNone (QVal value1) (QVal value2) (QVal value3)
                                                                        _  -> case cellOrientation entry3 of
                                                                            TL -> QNode (QVal value3) (QVal value1) QNone (QVal value2)
                                                                            _  -> QNode QNone (QVal value1) (QVal value3) (QVal value2)
                                                                    BL -> case cellOrientation entry2 of
                                                                        TL -> case cellOrientation entry3 of
                                                                            TR -> QNode (QVal value2) (QVal value3) (QVal value1) QNone
                                                                            _  -> QNode (QVal value2) QNone (QVal value1) (QVal value3)
                                                                        TR -> case cellOrientation entry3 of
                                                                            TL -> QNode (QVal value3) (QVal value2) (QVal value1) QNone
                                                                            _  -> QNode QNone (QVal value2) (QVal value1) (QVal value3)
                                                                        _  -> case cellOrientation entry3 of
                                                                            TL -> QNode (QVal value3) QNone (QVal value1) (QVal value2)
                                                                            _  -> QNode QNone (QVal value3) (QVal value1) (QVal value2)
                                                                    BR -> case cellOrientation entry2 of
                                                                        TL -> case cellOrientation entry3 of
                                                                            TR -> QNode (QVal value2) (QVal value3) QNone (QVal value1)
                                                                            _  -> QNode (QVal value2) QNone (QVal value3) (QVal value1)
                                                                        TR -> case cellOrientation entry3 of
                                                                            TL -> QNode (QVal value3) (QVal value2) QNone (QVal value1)
                                                                            _  -> QNode QNone (QVal value2) (QVal value3) (QVal value1)
                                                                        _ -> case cellOrientation entry3 of
                                                                            TL -> QNode (QVal value3) QNone (QVal value2) (QVal value1)
                                                                            _  -> QNode QNone (QVal value3) (QVal value2) (QVal value1)


                                                                [entry1@(row1,col1,value1),entry2@(row2,col2,value2)] -> case cellOrientation entry1 of
                                                                        TL -> case cellOrientation entry2 of
                                                                                TR -> QNode (QVal value1) (QVal value2) QNone QNone
                                                                                BL -> QNode (QVal value1) QNone (QVal value2) QNone
                                                                                _  -> QNode (QVal value1) QNone QNone (QVal value2)
                                                                        TR -> case cellOrientation entry2 of
                                                                                TL -> QNode (QVal value2) (QVal value1) QNone QNone
                                                                                BL -> QNode QNone (QVal value1) (QVal value2) QNone
                                                                                _  -> QNode QNone (QVal value1) QNone (QVal value2)
                                                                        BL -> case cellOrientation entry2 of
                                                                                TL -> QNode (QVal value2) QNone (QVal value1) QNone
                                                                                TR -> QNode QNone (QVal value2) (QVal value1) QNone
                                                                                _ -> QNode QNone QNone (QVal value1) (QVal value2)

                                                                [entry@(row,col,value)] -> case cellOrientation entry of
                                                                        TL -> QNode (QVal value) QNone QNone QNone
                                                                        TR -> QNode QNone (QVal value) QNone QNone
                                                                        BL -> QNode QNone QNone (QVal value) QNone
                                                                        BR -> QNode QNone QNone QNone (QVal value)

                                                                [] -> QNone
                                                        else if nonzeroes' == 0 then QNone
                                                             else let newRows = rows' `div` 2
                                                                      newColumns = columns' `div` 2 -- filter entries next
                                                                      topLeftCorner = topLeftCorner'
                                                                      topRightCorner = (topLeftRow,topLeftColumn + newColumns)
                                                                      bottomLeftCorner = (topLeftRow + newRows,topLeftColumn)
                                                                      bottomRightCorner = (topLeftRow + newRows, topLeftColumn + newColumns) in
                                                                          QNode (matrixMarketToQTreeHelper topLeftCorner (splitQMatrixMarket topLeftCorner mtx))
                                                                                (matrixMarketToQTreeHelper topRightCorner (splitQMatrixMarket topRightCorner mtx))
                                                                                (matrixMarketToQTreeHelper bottomLeftCorner (splitQMatrixMarket bottomLeftCorner mtx))
                                                                                (matrixMarketToQTreeHelper bottomRightCorner (splitQMatrixMarket bottomRightCorner mtx))   --split matrix

splitQMatrixMarket :: (Int, Int) -> MatrixMarket a -> MatrixMarket a
splitQMatrixMarket (row,column) MatrixMarket {rows = rows',
                                  columns = columns',
                                  nonzeroes = nonzeroes',
                                  entries = entries'} = let newRows = rows' `div` 2
                                                            newColumns = columns' `div` 2
                                                            newEntries = filter (\(entryRow,entryColumn,_) -> entryRow >= row &&
                                                                                                              entryColumn >= column &&
                                                                                                              entryRow < (row + newRows) &&
                                                                                                              entryColumn < (column + newColumns)) entries'
                                                            in
                                                                MatrixMarket {rows = newRows, columns = newColumns, nonzeroes = length newEntries, entries = newEntries}

qTreeToQMask :: QTree a -> MaskQTree
qTreeToQMask QNone = MQNone
qTreeToQMask (QVal _) = MQVal
qTreeToQMask QError = error "Can not turn QError to mask"
qTreeToQMask (QNode tl tr bl br) = MQNode (qTreeToQMask tl) (qTreeToQMask tr) (qTreeToQMask bl) (qTreeToQMask br)


compressMask :: MaskQTree -> MaskQTree
compressMask (MQNode MQVal MQVal MQVal MQVal) = MQVal
compressMask (MQNode tl tr bl br) = MQNode (compressMask tl) (compressMask tr) (compressMask bl) (compressMask br)
compressMask m = m

qTreeToBool :: QTree a -> QTree Bool
qTreeToBool QNone = QNone
qTreeToBool (QVal _) = QVal True
qTreeToBool QError = QError
qTreeToBool (QNode tl tr bl br) = QNode (qTreeToBool tl) (qTreeToBool tr) (qTreeToBool bl) (qTreeToBool br)