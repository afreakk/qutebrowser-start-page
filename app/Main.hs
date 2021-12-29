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
  homeDir <- fmap homeDirectory (getRealUserID >>= getUserEntryForID)
  quickmarks <- getQuickMarks homeDir
  bookmarks <- getBookmarks homeDir
  let maxRow = show $ max (length quickmarks) (length bookmarks)
  withConnection (homeDir ++ "/.local/share/qutebrowser/history.sqlite") (\conn -> do
    history <- query conn "SELECT url,title FROM History ORDER BY atime DESC LIMIT ?" (toRow [maxRow]):: IO [UrlTitle]
    frequentSites <- query conn "SELECT url,title FROM History GROUP BY url,title ORDER BY count(*) DESC LIMIT ?" (toRow [maxRow]) :: IO [UrlTitle]
    putStrLn $ renderHtml $ basicHtml history frequentSites quickmarks bookmarks)

link :: UrlTitle -> H.Html
link (UrlTitle url title) = H.li $ H.a H.! (A.href (H.toValue url)) H.! (A.class_ "text-ellipsis overflow-hidden block") $ H.toMarkup title

tw :: H.Html
tw =    H.script H.! (A.src "https://cdn.tailwindcss.com") $ "a"
-- tw =    H.script H.! (A.type_ "module") H.! (A.src "https://cdn.skypack.dev/twind/shim") $ "f"

header txt = H.span H.! A.class_ "font-medium text-sm font-mono mb-3 text-gray-400" $ txt

list listBody = H.ol H.! A.class_ "list-none whitespace-nowrap" $ listBody

container containerBody = H.div H.! A.class_ "h-full overflow-y-auto m-1 bg-black/65 max-w-sm align-top backdrop-blur-md" $ containerBody

basicHtml :: [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> H.Html
basicHtml history frequentSites quickmarks bookmarks = H.html $ do
  H.head $ do
  -- make webpage.darkmode behave
    H.meta H.! A.name "color-scheme" H.! A.content "dark light"
    H.title "qutebrowser-start-page"
    H.style css
    (tw)
  H.body H.! A.class_ "h-full object-cover bg-black grid grid-cols-4 text-slate-100 p-2 bg-[url('https://source.unsplash.com/random/?dark')]" $ do
    container $ do
      header "quickmarks"
      list $ mapM_ link quickmarks
    container $ do
      header "bookmarks"
      list $ mapM_ link bookmarks
    container $ do
      header "frequentSites"
      list $ mapM_ link frequentSites
    container $ do
      header "history"
      list $ mapM_ link history



css = ""
