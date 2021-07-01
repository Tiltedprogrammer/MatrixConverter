module Main where

import QTree ( readMtx, matrixMarketToQTree, qTreeToQMask, qTreeToBool )
import System.FilePath
import System.Directory
import Data.Char
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    path <- getCurrentDirectory
    let res = case args of
            [t,file] -> if map toLower t == "qtree" then do
                let qMtxPath = path </> "data" </> file
                qMtx <- readMtx qMtxPath
                print $ matrixMarketToQTree qMtx
                        else if map toLower t == "mask" then do
                            let qMtxPath = path </> "data" </> file
                            qMtx <- readMtx qMtxPath
                            print $ qTreeToQMask $ matrixMarketToQTree qMtx
                        else if map toLower t == "bool" then do
                            let qMtxPath = path </> "data" </> file
                            qMtx <- readMtx qMtxPath
                            print $ qTreeToBool $  matrixMarketToQTree qMtx
                        else error "qtree or mask are supported as the first argument"
            []          -> print "Transforms the matrix market file to QTree representation or to a Mask, run with\n mask/qtree 'file-path' as arguments \n 'file-path' is relative to data/"
            _           -> error "type and file are expected, e.g. stack run qtree file"
    res