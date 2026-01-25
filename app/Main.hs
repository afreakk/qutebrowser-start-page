{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

import Control.Applicative
import Database.SQLite.Simple (Only(..), query, withConnection)
import Database.SQLite.Simple.FromRow

import System.Posix.User

import Data.List (groupBy, nubBy, partition, sortBy)
import Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Network.URI (parseURI, uriAuthority, uriRegName, uriScheme)

import System.IO
import System.Environment
import System.Directory (doesFileExist)
import Control.Exception (try, IOException)

data UrlTitle = UrlTitle String String deriving (Show)

instance FromRow UrlTitle where
  fromRow = UrlTitle <$> field <*> field

lineToUrlTitle = wordsToUrlTitle . words
    where wordsToUrlTitle [] = UrlTitle "NoUrl" "NoTitle"
          wordsToUrlTitle [title] = UrlTitle "NoUrl" title
          wordsToUrlTitle [title,url] = UrlTitle url title
          wordsToUrlTitle (x:y:rest) = wordsToUrlTitle $ unwords [x,y] : rest  

reverseUrlTitle (UrlTitle url title) = UrlTitle (reverse url) (reverse title)

-- Safely read file, returning empty list if file doesn't exist or can't be read
safeReadLines :: FilePath -> IO [String]
safeReadLines path = do
  exists <- doesFileExist path
  if exists
    then do
      result <- try (readFile path) :: IO (Either IOException String)
      case result of
        Left _ -> return []
        Right contents -> return $ filter (not . null) (lines contents)
    else return []

getQuickMarks homeDir = do
  fileLines <- safeReadLines (homeDir ++ "/.config/qutebrowser/quickmarks")
  return $ map lineToUrlTitle fileLines

getBookmarks homeDir = do
  fileLines <- safeReadLines (homeDir ++ "/.config/qutebrowser/bookmarks/urls")
  return $ map (reverseUrlTitle . lineToUrlTitle . reverse) fileLines

-- Strip www. prefix from domain
stripWww :: String -> String
stripWww domain
  | take 4 domain == "www." = drop 4 domain
  | otherwise = domain

-- Extract domain from URL using Network.URI.
-- Returns empty string for non-http(s) URLs (about:, qute:, file:, etc.).
-- Strips www. prefix for consistent deduplication.
extractDomain :: String -> String
extractDomain url = fromMaybe "" $ do
  uri <- parseURI url
  let scheme = uriScheme uri
  if scheme == "http:" || scheme == "https:"
    then do
      auth <- uriAuthority uri
      return $ stripWww $ uriRegName auth
    else Nothing

-- Favicon URL - fetch directly from the site.
faviconUrl :: String -> Maybe String
faviconUrl url =
  let domain = extractDomain url
  in if null domain
     then Nothing
     else Just ("https://" ++ domain ++ "/favicon.ico")

-- Dedupe by domain, keeping first occurrence (which is highest count from SQL).
-- URLs without valid domains are kept individually.
dedupeByDomain :: [UrlTitle] -> [UrlTitle]
dedupeByDomain = nubBy sameDomain
  where
    sameDomain (UrlTitle u1 _) (UrlTitle u2 _) =
      let d1 = extractDomain u1
          d2 = extractDomain u2
      in not (null d1) && d1 == d2

-- Group extraction for quickmarks (split on first hyphen).
-- "home-flood" -> "home", "work-github-pr" -> "work", "ungrouped" -> "".
extractGroup :: String -> String
extractGroup title = case break (== '-') title of
  (group, '-':_) -> group
  _ -> ""

getGroup :: UrlTitle -> String
getGroup (UrlTitle _ title) = extractGroup title

-- Strip group prefix from title: "bok-erlend-1" with group "bok" -> "erlend-1"
stripGroupPrefix :: String -> UrlTitle -> UrlTitle
stripGroupPrefix group (UrlTitle url title) =
  UrlTitle url (drop (length group + 1) title)

-- Sort key that puts ungrouped items (empty group) at the end.
groupSortKey :: String -> (Int, String)
groupSortKey "" = (1, "")
groupSortKey g = (0, g)

-- Groups quickmarks by prefix; singleton groups are treated as ungrouped.
groupQuickmarks :: [UrlTitle] -> [(String, [UrlTitle])]
groupQuickmarks qms =
  let sorted = sortBy (comparing (groupSortKey . getGroup)) qms
      grouped = NE.groupBy (\a b -> getGroup a == getGroup b) sorted
      rawGroups = map (\(x :| xs) -> (getGroup x, x : xs)) grouped
      -- Separate real groups (2+ items) from singletons
      (realGroups, singletons) = partition (\(_, items) -> length items >= 2) rawGroups
      -- Merge singletons into ungrouped, treating them as ungrouped
      singletonItems = concatMap snd singletons
      -- Find existing ungrouped section if any
      (existingUngrouped, otherGroups) = partition (\(name, _) -> null name) realGroups
      ungroupedItems = concatMap snd existingUngrouped ++ singletonItems
      -- Rebuild with singletons merged into ungrouped at the end
  in otherGroups ++ (if null ungroupedItems then [] else [("", ungroupedItems)])

maxHistory :: Int
maxHistory = 50

maxFrequent :: Int
maxFrequent = 50

-- Multiplier for frequent sites query to ensure enough results after domain deduplication.
frequentQueryMultiplier :: Int
frequentQueryMultiplier = 3

-- Query history database, returning empty lists if database doesn't exist
getHistoryData :: FilePath -> IO ([UrlTitle], [UrlTitle])
getHistoryData dbPath = do
  exists <- doesFileExist dbPath
  if exists
    then do
      result <- try (withConnection dbPath $ \conn -> do
        history <- query conn "SELECT url,title FROM History ORDER BY atime DESC LIMIT ?" (Only maxHistory) :: IO [UrlTitle]
        frequentSitesRaw <- query conn "SELECT url,title FROM History GROUP BY url,title ORDER BY count(*) DESC LIMIT ?" (Only (maxFrequent * frequentQueryMultiplier)) :: IO [UrlTitle]
        let frequentSites = take maxFrequent $ dedupeByDomain frequentSitesRaw
        return (history, frequentSites)) :: IO (Either IOException ([UrlTitle], [UrlTitle]))
      case result of
        Left _ -> return ([], [])
        Right d -> return d
    else return ([], [])

main :: IO ()
main = do
  cssPath <- getArgs >>= \case
    (p:_) -> return p
    [] -> error "Usage: qutebrowser-start-page <css-path>"
  cssContent <- readFile cssPath
  homeDir <- fmap homeDirectory (getRealUserID >>= getUserEntryForID)
  quickmarks <- getQuickMarks homeDir
  bookmarks <- getBookmarks homeDir
  (history, frequentSites) <- getHistoryData (homeDir ++ "/.local/share/qutebrowser/history.sqlite")
  putStrLn $ renderHtml $ basicHtml cssContent history frequentSites quickmarks bookmarks

faviconImg :: String -> H.Html
faviconImg url = case faviconUrl url of
  Nothing -> return ()
  Just fav -> H.span H.! A.class_ "flex-shrink-0 w-4 h-4 bg-contain bg-center bg-no-repeat"
                     H.! A.style (H.toValue $ "background-image: url('" ++ fav ++ "')")
                     $ return ()

link :: UrlTitle -> H.Html
link (UrlTitle url title) = H.li $
  H.a H.! A.href (H.toValue url)
      H.! A.title (H.toValue title)
      H.! A.class_ "flex items-center gap-2 py-1.5 px-3 rounded hover:bg-white/10 hover:text-white text-slate-300 transition-colors duration-150" $ do
    faviconImg url
    H.span H.! A.class_ "truncate" $ H.toMarkup title

header clr txt = H.h2 H.! A.class_ (H.toValue $ "px-4 py-2.5 text-base font-semibold border-b border-white/10 sticky top-0 bg-slate-900/95 backdrop-blur-sm z-10 " ++ clr) $ txt

list listBody = H.ul H.! A.class_ "list-none p-1.5" $ listBody

container containerBody = H.div H.! A.class_ "h-full overflow-y-auto rounded-lg bg-slate-900/70 shadow-lg border border-white/5" $ containerBody

mkContainer :: String -> H.Html -> [UrlTitle] -> H.Html
mkContainer _ _ [] = return ()
mkContainer clr title links = container $ do
                          header clr title
                          list $ mapM_ link links

groupHeader :: String -> H.Html
groupHeader name = H.h3 H.! A.class_ "px-3 py-1.5 text-xs font-medium text-slate-500 uppercase tracking-wider mt-2 first:mt-0" $ H.toMarkup name

ungroupedDivider :: H.Html
ungroupedDivider = H.hr H.! A.class_ "border-t border-white/10 my-2 mx-3"

renderGroup :: Bool -> (String, [UrlTitle]) -> H.Html
renderGroup _ (groupName, items)
  -- Real groups get a header and stripped prefixes
  | not (null groupName) = do
      groupHeader groupName
      mapM_ (link . stripGroupPrefix groupName) items
-- Ungrouped items get a divider only if there are groups above
renderGroup hasGroupsAbove ("", items) = do
  if hasGroupsAbove then ungroupedDivider else return ()
  mapM_ link items

mkGroupedContainer :: String -> H.Html -> [UrlTitle] -> H.Html
mkGroupedContainer _ _ [] = return ()
mkGroupedContainer clr title links = container $ do
  header clr title
  let groups = groupQuickmarks links
      hasRealGroups = any (not . null . fst) groups
  list $ mapM_ (renderGroup hasRealGroups) groups


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
    (H.stringValue ("bg-gradient-to-br from-slate-950 via-slate-900 to-slate-950 text-slate-100 p-6 gap-4 h-full grid " ++ (countToGridCols colCount)))

basicHtml :: String -> [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> [UrlTitle] -> H.Html
basicHtml cssContent history frequentSites quickmarks bookmarks = do
  H.docType
  H.html $ do
    H.head $ do
      -- make webpage.darkmode behave
      H.meta H.! A.name "color-scheme" H.! A.content "dark light"
      H.style $ H.toMarkup cssContent
      H.title "qutebrowser-start-page"
    H.body H.! A.class_ (genBodyClasses history frequentSites quickmarks bookmarks) $ do
      mkGroupedContainer "text-amber-400" "Quickmarks" quickmarks
      mkContainer "text-emerald-400" "Bookmarks" bookmarks
      mkContainer "text-sky-400" "Frequent Sites" frequentSites
      mkContainer "text-violet-400" "History" history
