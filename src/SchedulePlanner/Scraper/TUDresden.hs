{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}
module SchedulePlanner.Scraper.TUDresden (scrapeTuDresden) where


import           Control.Arrow                (first, second, (&&&))
import           Control.Arrow.Unicode
import           Control.Monad                (join, mplus, (>=>))
import           Control.Monad.Unicode
import           Data.Bool                    (bool)
import qualified Data.Map                     as Map (Map, fromList, lookup)
import           Data.Maybe                   (catMaybes, fromMaybe, isJust,
                                               mapMaybe)
import           Data.Monoid.Unicode
import qualified Data.Text                    as T (Text, append, filter, pack,
                                                    splitOn, unpack)
import           Data.Text.ICU                (MatchOption (DotAll), Regex,
                                               find, findAll, group, regex)
import           Debug.Trace
import           Network.HTTP                 (Request_String,
                                               Response (Response), getRequest,
                                               rspBody, simpleHTTP)
import           Network.Stream               (Result)
import           Prelude.Unicode
import           SchedulePlanner.Scraper.Base
import           SchedulePlanner.Types        (Day (..), Lesson (..), Slot (..))
import           Text.Read                    (readMaybe)


grabTableRegex ∷ Regex
grabTableRegex = regex [DotAll] "<h1>(\\d). Semester</h1>.*?<table>(.*?)</table>"
trRegex        ∷ Regex
trRegex        = regex [DotAll] "<tr>(.*?)</tr>"
tdRegex        ∷ Regex
tdRegex        = regex [DotAll] "<td>(.*?)</td>"
aRegex         ∷ Regex
aRegex         = regex [DotAll] "<a .*?\">(.*?)</a>"
brRegex        ∷ Regex
brRegex        = regex [DotAll] " (.*?)<br ?/>"


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
    doIt i v@(Left _)
      | i <= 0    = return v
      | otherwise = a ≫= doIt (i-1)


tuDresdenRequestUrl ∷ Request_String
tuDresdenRequestUrl = getRequest "http://web.inf.tu-dresden.de/Fak/ss/15/studiengang/studiengang_inf_bach.html"


stdRetries ∷ Int
stdRetries = 4


getPage ∷ IO (Result (Response String))
getPage = simpleHTTP tuDresdenRequestUrl


ts a = traceShow a a

t a = trace a a

stripWhite ∷ T.Text → T.Text
stripWhite = T.filter (≢ ' ')


findRows ∷ T.Text → [T.Text]
findRows = mapMaybe (group 1) ∘ findAll trRegex


handleSubject ∷ [T.Text] → [Lesson T.Text]
handleSubject (a:_:_:_:_:_:b:c:d:_) =
  fromMaybe [] $ do
    name ← find aRegex a `mplus` find brRegex a ≫= group 1
    return $ fst $ foldr (flip $ uncurry (handleLesson name)) ([], 1) $ zip3 (splitBr b) (splitBr c) (splitBr d)
  where
    splitBr = join ∘ map (T.splitOn "<br/>") ∘ T.splitOn "<br />"

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
flatten2 (Just a) (Just b) = return (a,b)
flatten2 _        _        = Nothing


associateTables ∷ T.Text → [(Int, T.Text)]
associateTables source =

  cleanMaybes $ findAccociations <$> rawTables

  where

    cleanMaybes      = catMaybes ∘ fmap (uncurry flatten2)
    findAccociations = (group 1 >=> convertNumber) &&& group 2
    convertNumber    = readMaybe ∘ T.unpack
    rawTables        = findAll grabTableRegex source


toLesson ∷ [Int] → Response String → [Either String Semester]
toLesson n (Response { rspBody = r })
  | null n    = map (Right . Semester) $ mapMaybe (uncurry clean) tables
  | otherwise = do
    i ← n
    case join $ lookup i tables of
      Just c → return $ return $ Semester (i, c)
      Nothing → return $ Left $ "Cannot find semester " ⧺ show i
  -- maybe (Left "No parse") (Right . join . map handleSubject) $ lookup tables n
  where
    clean i (Just c) = return (i, c)
    clean i Nothing = Nothing

    associatedTables ∷ [(Int, T.Text)]
    associatedTables = associateTables $ T.pack r

    tables           ∷ [(Int, Maybe [Lesson T.Text])]
    tables           = fmap (second (fmap (join ∘ map handleSubject ∘ lessons) ∘ getRawLessons)) associatedTables

    getRawLessons    ∷ T.Text → Maybe [T.Text]
    getRawLessons    = fmap snd ∘ uncons ∘ findRows

    lessons          = fmap (mapMaybe (group 1) ∘ findAll tdRegex)


scrapeTuDresden ∷ [Int] → IO (Either String [Either String Semester])
scrapeTuDresden n = either (Left ∘ show) (Right ∘ toLesson n) <$> retry stdRetries getPage
