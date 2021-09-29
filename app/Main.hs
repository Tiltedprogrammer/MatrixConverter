{-# OPTIONS -Wall #-}
{-# LANGUAGE DeriveDataTypeable #-}


module Main where

import QTree
import System.FilePath
import System.Directory
import Data.Char
import System.Environment

import Data.Typeable
import Data.Data

import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import System.Console.GetOpt ( getOpt, usageInfo,
                               OptDescr(..), ArgDescr(..),  ArgOrder(..) )
import System.IO ( stderr, hPutStrLn, hPrint )

import Control.Monad ( when )



-- | Process command-line flags using the getOpt library
getFlags :: [String] -> IO (String, [Flag])
getFlags args =
    case getOpt Permute options args of
       (flags, [file], []) -> return (file, flags)
       (_, _, errs) -> cmdlnFail $ Just (concat errs)
    where
    cmdlnFail :: Maybe String -> IO a
    cmdlnFail msg = do
        case msg of
            Just s -> putStr $ "error: " ++ s
            _ -> return ()
        hPutStrLn stderr $ usageInfo header options
        exitFailure
        where header = "Usage: haskell2hardware-exe [options] file"


-- | Command-line flag details for the getOpt library
--
-- Define new command-line flags here and in the Flag type above
options :: [OptDescr Flag]
options =
    [ Option "q" ["qtree"] (OptArg QTree "bool") "Generate qtree representation of .mtx file, if 'bool' is specified Boolean qtree is dumped"
    , Option "m" ["mask"] (NoArg QMask) "Generate a mask from .mtx"
    , Option []  ["backend"] (ReqArg Backend "haskell|=poitin") "Choose backend: haskell for Haskell, poitin for distiller language"
    ]


-- | Types of command-line flags 
--
-- To add a new command-line flag, add it here and in the options list below

data Flag = QTree (Maybe String)
          | QMask
          | Backend String
        deriving (Eq,Typeable,Data,Show)



main :: IO ()
main = do
    args <- getArgs
    (sourceFilename, flags') <- getFlags args
    
    let 
        flags = case flags' of
            [] -> [(QTree Nothing),(Backend "haskell")]  -- Default flag if none given
            _ -> flags'

        -- Shorthand for testing a flag
        ifFlag f = when (toConstr f `elem` map toConstr flags)
        
        ifTreeBackend (Just b) = if b == "haskell" then HaskellTextTree else PoitinTextTree
        ifTreeBackend _ = HaskellTextTree

        ifMaskBackend (Just b) = if b == "haskell" then HaskellTextMask else PoitinTextMask
        ifMaskBackend _ = HaskellTextMask

        -- Get string associated with flag
        getFlagStr f = let fl = filter ((==toConstr f)  . toConstr) flags
                        in case fl of
                            [QTree s]     -> s
                            [Backend s]   -> Just s
                            _              -> Nothing
        -- Get any flag arguments
        qtreeArg = getFlagStr QTree{}
    
    qMtxIO <- readMtx sourceFilename

    let qMtx = matrixMarketToQTree qMtxIO 

    ifFlag QMask{} $ print . (ifMaskBackend (getFlagStr Backend{})) . compressMask . qTreeToQMask $ qMtx
    
    case getFlagStr QTree{} of
        Nothing -> ifFlag (QTree {}) $ print . (ifTreeBackend (getFlagStr Backend{})) $ qMtx
        _ -> ifFlag (QTree {}) $ print . (ifTreeBackend (getFlagStr Backend{})) . qTreeToBool $ qMtx