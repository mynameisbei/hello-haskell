module Lib where

import System.Console.GetOpt
import Data.Maybe ( fromMaybe )
import Data.Char
import System.IO

import System.Process

import System.FilePath

import System.Directory

import System.Environment

import Control.Applicative

data Cus' a b c = L c | W a b
    deriving(Show)

class Functor' f where
    fmap' :: (a -> b) -> f a -> f b

instance Functor' (Cus' a b)  where
    fmap' f (W a b) = W a b
    fmap' f (L x) = L (f x)


data Options = Options
    { optVerbose     :: Bool
    , optShowVersion :: Bool
    , optOutput      :: Maybe FilePath
    , optInput       :: Maybe FilePath
    , optLibDirs     :: [FilePath]
    } deriving Show

defaultOptions :: Options
defaultOptions    = Options
    { optVerbose     = False
    , optShowVersion = False
    , optOutput      = Nothing
    , optInput       = Nothing
    , optLibDirs     = []
    }

options :: [OptDescr (Options -> Options)]
options =
    [ Option ['v']     ["verbose"]
        (NoArg (\ opts -> opts { optVerbose = True }))
        "chatty output on stderr"
    , Option ['V','?'] ["version"]
        (NoArg (\ opts -> opts { optShowVersion = True }))
        "show version number"
    , Option ['o']     ["output"]
        (OptArg ((\ f opts -> opts { optOutput = Just f }) . fromMaybe "output")
                "FILE")
        "output FILE"
    , Option ['c']     []
        (OptArg ((\ f opts -> opts { optInput = Just f }) . fromMaybe "input")
                "FILE")
        "input FILE"
    , Option ['L']     ["libdir"]
        (ReqArg (\ d opts -> opts { optLibDirs = optLibDirs opts ++ [d] }) "DIR")
        "library directory"
    ]

compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
        (o,n,[]  ) -> return (foldl (flip ($)) defaultOptions o, n)
        (_,_,errs) -> ioError (userError (concat errs ++ usageInfo header options))
    where header = "Usage: ic [OPTION...] files..."

update :: IO()
update = do
    path <- getArgs
    (options, _) <- compilerOpts path
    case optInput options of
        Just p -> do
            paths <- fmap  (fmap $ mappend $ normalise p ++ "\\") (listDirectory $ normalise p)
            traverse exec paths
    return ()

exec :: FilePath -> IO[()]
exec path = traverse exec' [["reset", "--hard"], ["clean", "-df"], ["pull"]]
    where exec' args = do
            print path
            createProcess (proc "git" args) { cwd = Just $ normalise path }
            return ()
