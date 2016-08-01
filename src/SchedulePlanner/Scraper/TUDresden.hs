{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE UnicodeSyntax     #-}
module SchedulePlanner.Scraper.TUDresden (scrapeTuDresden) where


import           Control.Applicative          ((<|>))
import           Control.Applicative.Unicode
import           Control.Arrow                (second, (&&&))
import           Control.Arrow.Unicode
import           Control.Monad                (join, (>=>))
import           Control.Monad.Unicode
import           Data.Bool                    (bool)
import qualified Data.Map                     as Map (Map, empty, fromList,
                                                      fromListWith, lookup)
import           Data.Maybe                   (catMaybes, fromMaybe, mapMaybe)
import           Data.Monoid.Unicode          ((⊕))
import qualified Data.Text                    as T (Text, filter, pack, splitOn,
                                                    unpack, strip, isSuffixOf)
import           Network.HTTP                 (Request_String,
                                               Response (Response), getRequest,
                                               rspBody, simpleHTTP)
import           Network.Stream               (Result)
import           Prelude.Unicode
import           SchedulePlanner.Scraper.Base
import           SchedulePlanner.Types        (Day (..), Lesson (..), Slot (..))
import           Text.Read                    (readMaybe)
import           Text.XML.Cursor             hiding (bool)
import Text.XML (parseText_, Document)
import Control.Category
import Prelude hiding (id, (.))
import Data.Function ((&))


-- grabTableRegex ∷ Regex
-- grabTableRegex = regex [DotAll] "<h1>(\\d). Semester</h1>.*?<table>(.*?)</table>"
-- trRegex        ∷ Regex
-- trRegex        = regex [DotAll] "<tr>(.*?)</tr>"
-- tdRegex        ∷ Regex
-- tdRegex        = regex [DotAll] "<td>(.*?)</td>"
-- aRegex         ∷ Regex
-- aRegex         = regex [DotAll] "<a .*?\">(.*?)</a>"
-- brRegex        ∷ Regex
-- brRegex        = regex [DotAll] " (.*?)<br ?/>"


onlyIf :: a -> Bool -> Maybe a
onlyIf _ False = Nothing
onlyIf a _ = return a


bind = (>>=)


days ∷ Map.Map T.Text Int
days = Map.fromList
  [ ("Montag", 1)
  , ("Dienstag", 2)
  , ("Mittwoch", 3)
  , ("Donnerstag", 4)
  , ("Freitag", 5)
  , ("Samstag", 6)
  , ("Sonntag", 7)
  ]


uncons ∷ [a] → Maybe (a, [a])
uncons []     = Nothing
uncons (a:as) = Just (a,as)


retry ∷ Int → IO (Either a b) → IO (Either a b)
retry i a = a ≫= doIt i
  where
    doIt _ v@(Right _) = return v
    doIt i' v@(Left _)
      | i' <= 0    = return v
      | otherwise = a ≫= doIt (i'-1)


tuDresdenRequestUrl ∷ Request_String
tuDresdenRequestUrl = getRequest "http://web.inf.tu-dresden.de/Fak/ss/15/studiengang/studiengang_inf_bach.html"


stdRetries ∷ Int
stdRetries = 4


getPage ∷ IO (Result (Response String))
getPage = simpleHTTP tuDresdenRequestUrl


stripWhite ∷ T.Text → T.Text
stripWhite = T.filter (≢ ' ')


handleSubject ∷ [Cursor] → [Lesson T.Text]
handleSubject (a:_:_:_:_:_:b:c:d:_) =
  fst $ foldr (flip $ uncurry (handleLesson name)) ([], 1) $ zip3 (splitBr b) (splitBr c) (splitBr d)
  where
    name = (((a $/ element "a") >>= content) <|>
      (content a & head & T.splitOn "<br />")) & head :: T.Text
    splitBr = content >>> head >>> T.splitOn "<br />" >=> T.splitOn "<br/>"

handleSubject _ = []


handleLesson ∷ T.Text → [Lesson T.Text] → Int → (T.Text, T.Text, T.Text) → ([Lesson T.Text], Int)
handleLesson name other lectureNumber (ckind, cday, cslot) =

  uncurry (⁂) (maybe (id, id) ((:) ⁂ (+)) calculationResult) (other, lectureNumber)

  where
    calculationResult = do
      mday  ← Map.lookup (stripWhite cday) days
      rslot ← readMaybe $ T.unpack $ T.filter (not ∘ (∈ [' ', '.'])) cslot
      return (Lesson
              { subject  = name ⊕ identifier
              , day      = Day mday
              , timeslot = Slot rslot
              , weight   = 0
              }
            , bool 0 1 isLecture)

    isLecture  = stripWhite ckind ≡ "V"
    exerciseID = " UE"
    lectureID  = " VL" ⊕ T.pack (show lectureNumber)
    identifier = bool exerciseID lectureID isLecture


flatten2 ∷ Maybe a → Maybe b → Maybe (a, b)
flatten2 a b = (,) <$> a ⊛ b


associateTables ∷ Document → [(Int, Cursor)]
associateTables source =
  zip numbers tables

  where
    tables :: [Cursor]
    tables = flip map viableHeadings $
      ($| followingSibling
        >=> element "table")
      >>> head

    numbers :: [Int]
    numbers = map (content >>> head >>> T.splitOn "." >>> head >>> T.strip >>> T.unpack >>> read) viableHeadings

    viableHeadings =
      fromDocument source
      $// element "h1"
      >=> check (
        content >>> uncons >=>
          (fst >>> \heading ->
            heading `onlyIf` (". Semester" `T.isSuffixOf` heading))
      )



toLesson ∷ [Int] → Document → Map.Map Int (Either String Semester)
toLesson n doc =
  Map.fromList $ fmap (second (fmap Semester)) selected
  where
    selected
      | null n    =
        [(i, maybe (Left $ "Cannot find semester " ⊕ show i) return $ lookup i tables) | i <- n]
      | otherwise = fmap (second return) tables

    associatedTables ∷ [(Int, Cursor)]
    associatedTables = associateTables doc

    tables           ∷ [(Int, [Lesson T.Text])]
    tables           = map (uncurry handleTable) associatedTables

    handleTable index content =
      (index,) ∘ (lessons >>> handleSubject) <$> getRawLessons content

    getRawLessons    ∷ Cursor → [Cursor]
    getRawLessons    = drop 1 ∘ ($/ element "tr")

    lessons          = ($/ element "td")


scrapeTuDresden ∷ Scraper
scrapeTuDresden n = either (const errormap) (toLesson n . parseText_ .  rspBody) <$> retry stdRetries getPage
  where errormap = Map.fromList $ zip n (repeat (Left "ConnectionError"))
