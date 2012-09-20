import System.Environment
import System.Exit
import System.FilePath
import System.FilePath.GlobPattern (GlobPattern, (~~))
import System.IO

import System.OSX.FSEvents

import Control.Monad (forever, when)
import Control.Exception (bracket)

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
  in bracket
       (eventStreamCreate [dir] 1.0 True True True (handleEvent glob))
       (\es -> eventStreamDestroy es >> putStrLn "Bye Bye!")
       (const $ forever getLine)

handleEvent :: GlobPattern -> Event -> IO ()
handleEvent glob evt = 
  let fn      = takeFileName $ eventPath evt
      isMatch = isFileChange evt && fileMatchesGlob glob fn 
  in when isMatch $ putStrLn fn

fileMatchesGlob :: GlobPattern -> FilePath -> Bool
fileMatchesGlob glob fp =
  case glob of
    []  -> True
    "." -> True
    _   -> fp ~~ glob

isFileChange :: Event -> Bool
isFileChange evt = not $ hasFlag eventFlagItemRemoved 
                   && hasFlag eventFlagItemIsFile 
                   && any hasFlag [ eventFlagItemModified
                                  , eventFlagItemRenamed
                                  , eventFlagItemCreated
                                  ]
  where flags = eventFlags evt
        hasFlag f = flags .&. f /= 0
