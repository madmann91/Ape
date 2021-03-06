import Ape.Check
import Ape.Env
import Ape.Expr
import Ape.Parse
import Ape.Print
import Ape.Transform.CommonSubExpr
import Ape.Transform.PartialEval
import Ape.Transform.NormalizeExpr

import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import Text.Parsec
import Control.Monad
import System.Console.GetOpt
import System.Environment
import Data.Char

logo = unlines [
    "     .-\"-.            .-\"-.          .-\"-.             ",
    "   _/_-.-_\\_        _/.-.-.\\_      _/.-.-.\\_           ",
    "  / __} {__ \\      /|( o o )|\\    ( ( o o ) )           ",
    " / //  \"  \\\\ \\    | //  \"  \\\\ |    |/  \"  \\|     ",
    "/ / \\'---'/ \\ \\  / / \\'---'/ \\ \\    \\'/^\\'/       ",
    "\\ \\_/`\"\"\"`\\_/ /  \\ \\_/`\"\"\"`\\_/ /    /`\\ /`\\ ",
    " \\           /    \\           /    /  /|\\  \\          "]

data Flag = OptLevel Int
data Options = Options { optLevel :: Int
                       , files :: [String]
                       } deriving Show

defaultOptions :: [String] -> Options
defaultOptions f = Options { optLevel = 3, files = f }

options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option []    ["opt-level"] (ReqArg readOptLevel "level") "Optimisation level"
    , Option ['h'] ["help"] (NoArg showHelp) "Show help"
    ]
    where
        readOptLevel arg opt = return opt { optLevel = read arg }
        showHelp opt = do
            putStrLn logo
            putStrLn (usageInfo "ApeMachine [options] files..." options)
            return opt

parseOptions :: IO Options
parseOptions = do
    args <- getArgs
    case getOpt RequireOrder options args of
        ([], [], []) -> return $ defaultOptions []
        (actions, nonOptions, []) -> foldl (>>=) (return $ defaultOptions nonOptions) actions
        (_, _, errors) -> parsingError errors
    where
        parsingError errors = do
            mapM_ (\(x:xs) -> putStr $ (toUpper x):xs) errors
            return $ defaultOptions []

optimize :: Int -> Expr Info -> Expr Info
optimize level ast =
    case level of
        3 -> partialEval emptyEnv ast'
        1 -> commonSubExpr emptyExprMap emptyEnv ast'
        0 -> ast
        _ -> ast'
    where
        ast' = optimize (level - 1) ast

compileFile :: Int -> String -> IO ()
compileFile opt file = do
    bytes <- BS.readFile file
    let text = E.decodeUtf8 bytes
    case parse irParser file text of
        Left err -> print err
        Right exprs -> forM_ exprs (\ast ->
            case check emptyEnv ast of
                Left msg -> putStrLn msg
                Right ast' -> do
                    let optAST = optimize opt ast'
                    putStrLn $ prettyPrint0 optAST)

main = do
    opts <- parseOptions
    case opts of
        Options _ [] -> putStrLn "No input files."
        Options l _ | l > 3 || l < 0 -> putStrLn "Unsupported optimization level."
        Options l f -> mapM_ (compileFile $ optLevel opts) (files opts)
