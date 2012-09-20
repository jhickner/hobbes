import System.Environment (getArgs)
import System.Exit
import System.FilePath
import System.FilePath.GlobPattern (GlobPattern, (~~))

import System.OSX.FSEvents

import Control.Monad (forever, when)
import Control.Exception (bracket)

import Data.Bits ((.&.))


main :: IO ()
main = do
  args <- getArgs
  case args of
    (path:_) -> runWatcher path
    _        -> usage

usage :: IO ()
usage = putStrLn "Usage: hobbes [path]" >> exitSuccess

runWatcher :: FilePath -> IO ()
runWatcher path =
  let (dir, glob) = splitFileName path
  in bracket
       (eventStreamCreate [dir] 1.0 True True True (handleEvent glob))
       (\es -> putStrLn "Bye Bye!" >> eventStreamDestroy es)
       (const $ forever getLine)

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

handleEvent :: GlobPattern -> Event -> IO ()
handleEvent glob evt = 
  let fn      = takeFileName $ eventPath evt
      isMatch = isFileChange evt && fileMatchesGlob glob fn 
  in when isMatch $ putStrLn fn
