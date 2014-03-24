module Main where

import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.GlobPattern (GlobPattern, (~~))
import System.IO

import Filesystem.Path.CurrentOS (fromText, encodeString)

import System.FSNotify

import Control.Monad (forever)
import Control.Concurrent (threadDelay)

import Data.Text (pack)
import Data.Bits ((.&.))


main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  getArgs >>= parse >>= runWatcher

parse ::  [String] -> IO FilePath
parse ["-h"]   = usage >> exitSuccess
parse []       = return "."
parse (path:_) = return path

usage :: IO ()
usage = putStrLn "Usage: hobbes [path]"

runWatcher :: FilePath -> IO ()
runWatcher path =
  let (dir, glob) = splitFileName path
  in withManager $ \m -> do
       watchTree m (fromText $ pack dir) (globModified glob) printPath
       forever $ threadDelay 1000000

globModified :: GlobPattern -> Event -> Bool
globModified glob evt@(Modified _ _) = matchesGlob glob evt
globModified _ _ = False

matchesGlob :: GlobPattern -> Event -> Bool
matchesGlob glob = fileMatchesGlob glob . takeFileName . encodeString . eventPath

printPath :: Event -> IO ()
printPath = putStrLn . encodeString . eventPath 

fileMatchesGlob :: GlobPattern -> FilePath -> Bool
fileMatchesGlob []   _  = True
fileMatchesGlob "."  _  = True
fileMatchesGlob glob fp = fp ~~ glob
