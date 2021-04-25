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

data UrlTitle = UrlTitle String String deriving (Show)

instance FromRow UrlTitle where
  fromRow = UrlTitle <$> field <*> field

lineToUrlTitle = wordsToUrlTitle . words
    where wordsToUrlTitle [] = UrlTitle "NoUrl" "NoTitle"
          wordsToUrlTitle [url] = UrlTitle url "NoTitle"
          wordsToUrlTitle [url,title] = UrlTitle url title
          wordsToUrlTitle (x:y:rest) = wordsToUrlTitle $ unwords [x,y] : rest  

reverseUrlTitle (UrlTitle url title) = UrlTitle title url

getQuickMarks homeDir = do 
  contents <- readFile (homeDir ++ "/.config/qutebrowser/quickmarks")
  return $ map (reverseUrlTitle . lineToUrlTitle) (lines contents)

getBookmarks homeDir = do 
  contents <- readFile (homeDir ++ "/.config/qutebrowser/bookmarks/urls")
  return $ map lineToUrlTitle (lines contents)

main :: IO ()
main = do
  homeDir <- fmap homeDirectory (getRealUserID >>= getUserEntryForID)
  quickmarks <- getQuickMarks homeDir
  bookmarks <- getBookmarks homeDir
  let maxRow = show $ max (length quickmarks) (length bookmarks)
  withConnection (homeDir ++ "/.local/share/qutebrowser/history.sqlite") (\conn -> do
    history <- query conn "SELECT url,title FROM History ORDER BY atime DESC LIMIT ?" (toRow [maxRow]):: IO [UrlTitle]
    frequentSites <- query conn "SELECT url,title FROM History GROUP BY url,title ORDER BY count(*) DESC LIMIT ?" (toRow [maxRow]) :: IO [UrlTitle]
    putStrLn $ renderHtml $ basicHtml history frequentSites quickmarks bookmarks)

link :: UrlTitle -> H.Html
link (UrlTitle url title) = H.a H.! A.href (H.toValue url) $ H.toMarkup title

basicHtml :: [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> H.Html
basicHtml history frequentSites quickmarks bookmarks = H.html $ do
  H.head $ do
    H.title "qutebrowser-start-page"
    H.style css
  H.body $ do
    H.div $ do
      H.h1 "quickmarks"
      mapM_ link quickmarks
    H.div $ do
      H.h1 "bookmarks"
      mapM_ link bookmarks
    H.div $ do
      H.h1 "frequentSites"
      mapM_ link frequentSites
    H.div $ do
      H.h1 "history"
      mapM_ link history



css = "\
\body {\
\  display: flex;\
\  flex-wrap: wrap;\
\}\
\\
\div {\
\  display: flex;\
\  padding: 10px;\
\  flex-direction: column;\
\}\
\a {\
\  text-decoration: none;\
\}\
\"
