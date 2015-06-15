{-# LANGUAGE OverloadedStrings #-}
module SchedulePlanner.Scraper.TUDresden (scrapeTuDresden) where


import           Control.Arrow
import           Control.Monad
import           Data.Bool
import qualified Data.Map                   as Map
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Text.ICU
import           Debug.Trace                (traceShow)
import           Network.HTTP
import           Network.Stream
import           SchedulePlanner.Calculator (Day (..), Lesson (..), Slot (..))


grabTableRegex semester = regex [DotAll] $ "<h1>" `T.append` T.pack (show semester) `T.append` ". Semester</h1>.*?<table>(.*?)</table>"
trRegex = regex [DotAll] "<tr>(.*?)</tr>"
tdRegex = regex [DotAll] "<td>(.*?)</td>"
aRegex  = regex [DotAll] "<a .*?\">(.*?)</a>"
brRegex = regex [DotAll] " (.*?)<br ?/>"


days :: Map.Map T.Text Int
days = Map.fromList
  [ ("Montag", 1)
  , ("Dienstag", 2)
  , ("Mittwoch", 3)
  , ("Donnerstag", 4)
  , ("Freitag", 5)
  , ("Samstag", 6)
  , ("Sonntag", 7)
  ]


uncons :: [a] -> Maybe (a, [a])
uncons [] = Nothing
uncons (a:as) = Just (a,as)


tuDresdenRequestUrl :: Request_String
tuDresdenRequestUrl = getRequest "http://web.inf.tu-dresden.de/Fak/ss/15/studiengang/studiengang_inf_bach.html"


stdRetries :: Int
stdRetries = 4


getPage :: IO (Result (Response String))
getPage = simpleHTTP tuDresdenRequestUrl


handleSubject :: [T.Text] -> [Lesson T.Text]
handleSubject (a:_:_:_:_:_:b:c:d:_) =
  maybe [] id $ do
    name <- maybe (find brRegex a) Just (find aRegex a) >>= group 1
    return $ fst $ foldr (flip $ uncurry (handleLesson name)) ([], 1) $ zip3 (splitBr b) (splitBr c) (splitBr d)
  where
    splitBr = join . map (T.splitOn "<br/>") . T.splitOn "<br />"

handleSubject _ = []


handleLesson :: T.Text -> [Lesson T.Text] -> Int -> (T.Text, T.Text, T.Text) -> ([Lesson T.Text], Int)
handleLesson name other lectureNumber (ckind, cday, cslot) =
  uncurry (***) (maybe (id, id) ((:) *** (+)) calculationResult) (other, lectureNumber)
  where
    calculationResult = constructLesson <$> Map.lookup (T.replace " " "" cday) days

    constructLesson mday =
      ( Lesson
        { subject  = name `T.append` identifier
        , day      = Day mday
        , timeslot = Slot $ read $ T.unpack $ T.replace "." "" $ T.replace " " "" cslot
        }
      , bool 0 1 isLecture)
    isLecture  = ckind == "V"
    exerciseID = "UE"
    lectureID  = " VL" `T.append` T.pack (show lectureNumber)
    identifier = bool exerciseID lectureID isLecture
handleLesson _ other lectureNumber a = (other, lectureNumber)


justShow :: Show a => a -> a
justShow a = traceShow a a


toLesson :: Int -> Response String -> Either String [Lesson T.Text]
toLesson n (Response { rspBody = r }) = maybe (Left "No parse") (Right . join . map handleSubject) lessons
  where
    table   = find (grabTableRegex n) (T.pack r) >>= group 1
    rawLessons = snd <$> ((catMaybes . map (group 1) . findAll trRegex <$> table) >>= uncons)
    lessons = map (\a -> traceShow (Prelude.length a) a) <$> map (catMaybes . map (group 1) . findAll tdRegex) <$> rawLessons


scrapeTuDresden :: Int -> IO (Either String [Lesson T.Text])
scrapeTuDresden n = getPage >>= retry stdRetries
  where
    retry :: Int -> Result (Response String) -> IO (Either String [Lesson T.Text])
    retry _ (Right v) = return $ toLesson n v
    retry 0 (Left err) = return $ Left $ show err
    retry i _ = getPage >>= retry (i-1)

