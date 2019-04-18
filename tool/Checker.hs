{-# LANGUAGE DeriveDataTypeable, TypeSynonymInstances #-}

import Parser
import WitnessTree

import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Tree
import System.Environment
import System.FilePath.Posix
import System.Process
import System.Console.CmdArgs
import GHC.IO.Handle
import Control.Monad
import Data.Text (strip, pack, unpack)
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import Text.Printf (printf)

-- DEBUG
import System.IO.Unsafe
import Debug.Trace

writeToFile :: FilePath -> String -> IO()
writeToFile file content = writeFile file content

data Interaction = Passive 
                 | Interactive
                 deriving (Data,Typeable,Show,Eq)

data SubtypingMode = GenAsync
                   | GenAll
                   | TestAsync
                   deriving (Data,Typeable,Show,Eq)

data Subtyping = Subtyping 
                 { interaction :: Interaction
                 , typingmode :: SubtypingMode
                 , sub :: String
                 , sup :: String
                 , bound :: Int
                 , nomin :: Bool
                 }
               deriving (Data,Typeable,Show,Eq)
submodes =  enum
           [ 
             TestAsync &= help "Does not print pictures" &= name "T"
           ]


subargs = Subtyping 
 { interaction = enum [ Passive &= help "Passive mode (arguments are paths to files)"
                      , Interactive &= help "Interactive mode (arguments are given in line)"
                      ]
 , typingmode = submodes
 , sub = def  &= argPos 0  &= typ "FILE/LOCALTYPE" 
 , sup = def &= argPos 1  &= typ "FILE/LOCALTYPE"
 , bound = def &= opt "30"  &= argPos 2  &= typ "INT"
 , nomin = def
           &= explicit &= name "nomin"
           &= help "Don't minimize machines"      
 }  &= help "Asynchronous session subtyping"



getLocalTypeString :: Interaction -> String -> IO String
getLocalTypeString Passive s = readFile s
getLocalTypeString Interactive s = return s


main :: IO ()
main = do 
  pargs <- cmdArgs (modes [subargs]) 
  subinp <- getLocalTypeString (interaction pargs) (sub pargs)
  supinp <- getLocalTypeString (interaction pargs) (sup pargs)
  case parseLocalType subinp of
    Left err -> print err
    Right subans -> 
      case parseLocalType supinp of
        Left err -> print err
        Right supans ->
          if not (wellFormed subans && wellFormed supans)
          then putStrLn "Error in local type (not well-formed)."
          else 
            do
              --
              when ((typingmode pargs) == GenAsync ||
                    ((typingmode pargs) == GenAll) || (typingmode pargs) == TestAsync ) $
                do
                  start <- getCurrentTime
                  --
                  checkingAlgorithm ((typingmode pargs) /= TestAsync )  (nomin pargs) (bound pargs) subans supans
                  --
                  end <- getCurrentTime
                  if (typingmode pargs) /= TestAsync
                    then putStrLn $ (show $ diffUTCTime end start)
                    else return ()
              return ()
  
  
  
