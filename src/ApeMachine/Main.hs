import Ape.Parse
import Ape.Check
import Ape.Env
import Ape.Eval
import Ape.Expr

import qualified Data.Text.Encoding as E
import qualified Data.ByteString as BS
import Text.Parsec
import Control.Monad

logo = unlines [
    "     .-\"-.            .-\"-.          .-\"-.             ",
    "   _/_-.-_\\_        _/.-.-.\\_      _/.-.-.\\_           ",
    "  / __} {__ \\      /|( o o )|\\    ( ( o o ) )           ",
    " / //  \"  \\\\ \\    | //  \"  \\\\ |    |/  \"  \\|     ",
    "/ / \\'---'/ \\ \\  / / \\'---'/ \\ \\    \\'/^\\'/       ",
    "\\ \\_/`\"\"\"`\\_/ /  \\ \\_/`\"\"\"`\\_/ /    /`\\ /`\\ ",
    " \\           /    \\           /    /  /|\\  \\          "]

main = do
    putStrLn logo
    bytes <- BS.readFile "input.ape"
    let text = E.decodeUtf8 bytes
    let ast = parse irParser "input.ape" text
    case ast of
        Right list -> do
            print ast
            forM_ list (\e -> case check emptyEnv e of
                Right t -> print t
                Left msg -> print msg)
        Left msg -> print msg
