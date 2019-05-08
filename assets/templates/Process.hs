{-# LANGUAGE FlexibleContexts #-}

import Control.Monad

import System.IO
import System.Random
import System.Directory

import Text.Parsec.Char
import Text.Parsec
import Text.Parsec.String (Parser(..))

assetsDir = "assets/content/"
orgDir = "assets/org/"

main = do
    putStrLn "Starting Haskell process..."
    files <- getDirectoryContents orgDir
    let removedIntrs = map ((++) orgDir) $ filter (\x -> not $ any ((==) x) ["Interests.org", ".", ".."]) files
        len = length removedIntrs
    entries <- mapM (liftM show . readOrgFile) removedIntrs
    withFile "src/Reads.elm" AppendMode $ \h -> do
        hPutStr h $ "\nreads = [" ++ concat entries ++ "]"
        hClose h

readOrgFile :: FilePath -> IO Entry
readOrgFile fname = do
    handle <- openFile fname ReadMode
    t <- hGetLine handle
    d <- hGetLine handle
    hClose handle
    let pRes = parse parseOrgHeader "" $ unlines [t, d]
    case pRes of
        Left err -> do
            putStrLn $ show err
            return $ Entry "" "" "" ""
        Right res ->
            genEntry res fname

genEntry (ttl, dt) fname = do
    i <- getStdRandom $ randomR (0, length images - 1)
    return $ Entry
        { title = ttl
        , date = dt
        , url = urlFromName fname
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
        let
            withQuotes f = show . f
        in
        "{ title = " ++ withQuotes title e ++
        ", date = " ++ withQuotes date e ++
        ", url = " ++ withQuotes url e ++
        ", imgsrc = " ++ withQuotes imgsrc e ++
        " }"

parseOrgHeader :: Parser (String, String)
parseOrgHeader =
    let
        p h = string ("# " ++ h ++ ": ") *> (many $ noneOf "\n")
        t h = string ("\n# " ++ h ++ ": ") *> (many $ noneOf "\n")
    in do
        (,) <$> p "TITLE" <*> t "DATE"

urlFromName :: String -> String
urlFromName s = '/' : map (\c -> if c == ' ' then '-' else c) strippedLink
  where
    strippedLink = flip (++) ".md" $ reverse $ drop 4 $ takeWhile (not . (==) '/') $ reverse s
        
images =
    [ "https://images.freeimages.com/images/small-previews/ffa/water-lilly-1368676.jpg"
    , "https://images.freeimages.com/images/small-previews/535/natural-wonders-1400924.jpg"
    , "https://images.freeimages.com/images/small-previews/0cf/tulips-1-1377350.jpg"
    ]
