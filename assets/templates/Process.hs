{-# LANGUAGE FlexibleContexts #-}

import Control.Monad

import System.IO
import System.Random
import System.Directory

import Text.Parsec.Char
import Text.Parsec
import Text.Parsec.String (Parser(..))

main = do
    files <- getDirectoryContents "../content"
    let removedIntrs = filter (not . (==) "Interests.md") files
        len = length removedIntrs
    entries <- mapM (liftM show . readOrgFile) $ removedIntrs
    print entries
    appendFile "../../src/Reads.elm" $ "\nreads = " ++ show entries
    return ()

readOrgFile :: FilePath -> IO Entry
readOrgFile fname = do
    handle <- openFile fname ReadMode
    t <- hGetLine handle
    d <- hGetLine handle
    let pRes = parse parseOrgHeader "" $ unlines [t, d]
    case pRes of
        Left _ -> do
            putStrLn "Parse Error!"
            return $ Entry "" "" "" ""
        Right res ->
            genEntry res

genEntry (ttl, dt) = do
    i <- getStdRandom $ randomR (0, length images - 1)
    return $ Entry
        { title = ttl
        , date = dt
        , url = urlFromTitle ttl
        , imgsrc = images !! i
        }
    
-- reflected entry type to Elm
data Entry = Entry
    { title :: String
    , date :: String
    , url :: String
    , imgsrc :: String
    }

instance Show Entry where
    show e =
        "{ title = " ++ title e ++
        ", date = " ++ date e ++
        ", url = " ++ url e ++
        ", imgsrc = " ++ imgsrc e ++
        " }"

parseOrgHeader :: Parser (String, String)
parseOrgHeader =
    let
        p h = string ("# " ++ h ++ ": ") *> (many $ noneOf "\n")
    in do
        (,) <$> p "TITLE" <*> p "DATE"

urlFromTitle :: String -> String
urlFromTitle s = "/Reads/" ++ s
        
images =
    [ "https://images.freeimages.com/images/small-previews/ffa/water-lilly-1368676.jpg"
    , "https://images.freeimages.com/images/small-previews/535/natural-wonders-1400924.jpg"
    , "https://images.freeimages.com/images/small-previews/0cf/tulips-1-1377350.jpg"
    ]
