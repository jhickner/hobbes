module Main where

import           Control.Concurrent.Chan     (getChanContents, newChan)
import           Data.Monoid                 ((<>))
import           Options.Applicative         (Parser, argument, execParser,
                                              fullDesc, header, helper, info,
                                              many, metavar, progDesc, str)
import           System.Exit                 (exitSuccess)
import           System.FilePath             (splitFileName, takeFileName)
import           System.FilePath.GlobPattern (GlobPattern, (~~))
import           System.FSNotify             (Event (..), eventPath,
                                              watchTreeChan, withManager)
import           System.IO                   (BufferMode (NoBuffering),
                                              hSetBuffering, stdout)

data Options = Options { paths :: [FilePath] }

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  getOptions >>= runWatcher
  exitSuccess

getOptions :: IO Options
getOptions = execParser opts
  where
    opts =
      info (helper <*> optionsParser)
      ( fullDesc
        <> progDesc "Echoes the filenames of modified files to stdout, \
                    \one file per line."
        <> header "hobbes - a file activity monitor"
      )

optionsParser :: Parser Options
optionsParser =  Options <$> (many . argument str . metavar $ "PATHS..")

runWatcher :: Options -> IO ()
runWatcher (Options ps) =
  withManager $ \m -> do
    chan <- newChan
    mapM_ (watchPath m chan) ps
    getChanContents chan >>= mapM_ printPath
  where
    watchPath manager chan path =
      let (dir, glob) = splitFileName path
      in watchTreeChan manager dir (globModified glob) chan

globModified :: GlobPattern -> Event -> Bool
globModified _    (Removed _ _) = False
globModified glob evt           = matchesGlob glob evt

matchesGlob :: GlobPattern -> Event -> Bool
matchesGlob glob = fileMatchesGlob glob . takeFileName .  eventPath

printPath :: Event -> IO ()
printPath = putStrLn . eventPath

fileMatchesGlob :: GlobPattern -> FilePath -> Bool
fileMatchesGlob ""   _  = True
fileMatchesGlob "."  _  = True
fileMatchesGlob glob fp = fp ~~ glob
