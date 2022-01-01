{-# LANGUAGE OverloadedStrings #-}
module Main where
import Control.Monad (forM_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

import Control.Applicative
import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

import System.Posix.User

import Data.List

import System.IO
import System.Environment

data UrlTitle = UrlTitle String String deriving (Show)

instance FromRow UrlTitle where
  fromRow = UrlTitle <$> field <*> field

lineToUrlTitle = wordsToUrlTitle . words
    where wordsToUrlTitle [] = UrlTitle "NoUrl" "NoTitle"
          wordsToUrlTitle [title] = UrlTitle "NoUrl" title
          wordsToUrlTitle [title,url] = UrlTitle url title
          wordsToUrlTitle (x:y:rest) = wordsToUrlTitle $ unwords [x,y] : rest  

reverseUrlTitle (UrlTitle url title) = UrlTitle (reverse url) (reverse title)

getQuickMarks homeDir = do 
  contents <- readFile (homeDir ++ "/.config/qutebrowser/quickmarks")
  return $ map lineToUrlTitle (lines contents)

getBookmarks homeDir = do 
  contents <- readFile (homeDir ++ "/.config/qutebrowser/bookmarks/urls")
  return $ map (reverseUrlTitle . lineToUrlTitle . reverse) (lines contents)

main :: IO ()
main = do
  cssPath <- fmap head getArgs
  homeDir <- fmap homeDirectory (getRealUserID >>= getUserEntryForID)
  quickmarks <- getQuickMarks homeDir
  bookmarks <- getBookmarks homeDir
  let maxRow = show $ max (length quickmarks) (length bookmarks)
  withConnection (homeDir ++ "/.local/share/qutebrowser/history.sqlite") (\conn -> do
    history <- query conn "SELECT url,title FROM History ORDER BY atime DESC LIMIT ?" (toRow [maxRow]):: IO [UrlTitle]
    frequentSites <- query conn "SELECT url,title FROM History GROUP BY url,title ORDER BY count(*) DESC LIMIT ?" (toRow [maxRow]) :: IO [UrlTitle]
    putStrLn $ renderHtml $ basicHtml cssPath history frequentSites quickmarks bookmarks)

link :: UrlTitle -> H.Html
link (UrlTitle url title) = H.li $ H.a H.! (A.href (H.toValue url)) H.! (A.class_ "hover:text-gray-200 text-ellipsis overflow-hidden block") $ H.toMarkup title

header txt = H.span H.! A.class_ "w-full block text-center bg-black/65 sticky top-0 backdrop-blur-sm font-bold text-lg font-mono text-gray-400" $ txt

list listBody = H.ol H.! A.class_ "list-none whitespace-nowrap" $ listBody

container containerBody = H.div H.! A.class_ "h-full overflow-y-auto m-4 bg-black/65 align-top backdrop-blur-md" $ containerBody

mkContainer :: H.Html -> [UrlTitle] -> H.Html
mkContainer title [] = return ()
mkContainer title links = container $ do
                          header title
                          list $ mapM_ link links


genBodyClasses :: [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> H.AttributeValue
genBodyClasses history frequentSites quickmarks bookmarks =
  let
    cc [] = 0
    cc _ = 1
    colCount =  foldl (+) 0 (map cc [history, frequentSites, quickmarks, bookmarks])
    -- need to literally write all the cols, so tailwind can see it
    countToGridCols 1 = "grid-cols-1"
    countToGridCols 2 = "grid-cols-2"
    countToGridCols 3 = "grid-cols-3"
    countToGridCols 4 = "grid-cols-4"
    countToGridCols _ = "should-not-be-more-than-4-cols-possible"
  in
    (H.stringValue ("bg-[url('https://source.unsplash.com/random/?night')] text-slate-100 p-10 h-full bg-cover bg-black grid " ++ (countToGridCols colCount)))

basicHtml :: String -> [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> H.Html
basicHtml cssPath history frequentSites quickmarks bookmarks =
  H.html $ do
    H.head $ do
      -- make webpage.darkmode behave
      H.meta H.! A.name "color-scheme" H.! A.content "dark light"
      (H.link H.! (A.href (H.stringValue cssPath))) H.! (A.rel "stylesheet")
      H.title "qutebrowser-start-page"
    H.body H.! A.class_ (genBodyClasses history frequentSites quickmarks bookmarks) $ do
      mkContainer "Quickmarks" quickmarks
      mkContainer "Bookmarks" bookmarks
      mkContainer "Frequent Sites" frequentSites
      mkContainer "History" history

