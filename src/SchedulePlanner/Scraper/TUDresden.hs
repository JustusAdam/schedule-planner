{-# LANGUAGE OverloadedStrings #-}
module SchedulePlanner.Scraper.TUDresden (scrapeTuDresden) where


import           Control.Arrow
import           Control.Monad
import           Data.Bool
import qualified Data.Map              as Map
import           Data.Maybe
import qualified Data.Text             as T
import           Data.Text.ICU
import           Network.HTTP
import           Network.Stream
import           SchedulePlanner.Types (Day (..), Lesson (..), Slot (..))


grabTableRegex :: Int -> Regex
grabTableRegex = regex [DotAll] . T.append "<h1>" . flip T.append ". Semester</h1>.*?<table>(.*?)</table>" . T.pack . show
trRegex :: Regex
trRegex = regex [DotAll] "<tr>(.*?)</tr>"
tdRegex :: Regex
tdRegex = regex [DotAll] "<td>(.*?)</td>"
aRegex  :: Regex
aRegex  = regex [DotAll] "<a .*?\">(.*?)</a>"
brRegex :: Regex
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
uncons []     = Nothing
uncons (a:as) = Just (a,as)


retry :: Int -> IO (Either a b) -> IO (Either a b)
retry i a = a >>= doIt i
  where
    doIt _ v@(Right _) = return v
    doIt i v@(Left _)
      | i <= 0    = return v
      | otherwise = a >>= doIt (i-1)


tuDresdenRequestUrl :: Request_String
tuDresdenRequestUrl = getRequest "http://web.inf.tu-dresden.de/Fak/ss/15/studiengang/studiengang_inf_bach.html"


stdRetries :: Int
stdRetries = 4


getPage :: IO (Result (Response String))
getPage = simpleHTTP tuDresdenRequestUrl


handleSubject :: [T.Text] -> [Lesson T.Text]
handleSubject (a:_:_:_:_:_:b:c:d:_) =
  fromMaybe [] $ do
    name <- find brRegex a `mplus` find aRegex a >>= group 1
    return $ fst $ foldr (flip $ uncurry (handleLesson name)) ([], 1) $ zip3 (splitBr b) (splitBr c) (splitBr d)
  where
    splitBr = join . map (T.splitOn "<br/>") . T.splitOn "<br />"

handleSubject _ = []


handleLesson :: T.Text -> [Lesson T.Text] -> Int -> (T.Text, T.Text, T.Text) -> ([Lesson T.Text], Int)
handleLesson name other lectureNumber (ckind, cday, cslot) =
  uncurry (***) (maybe (id, id) ((:) *** (+)) calculationResult) (other, lectureNumber)
  where
    calculationResult = constructLesson <$> Map.lookup (T.filter (== ' ') cday) days

    constructLesson mday =
      ( Lesson
        { subject  = name `T.append` identifier
        , day      = Day mday
        , timeslot = Slot $ read $ T.unpack $ T.filter (`elem` [' ', '.']) cslot
        , weight   = 0
        }
      , bool 0 1 isLecture)
    isLecture  = ckind == "V"
    exerciseID = " UE"
    lectureID  = " VL" `T.append` T.pack (show lectureNumber)
    identifier = bool exerciseID lectureID isLecture
handleLesson _ other lectureNumber _ = (other, lectureNumber)


toLesson :: Int -> Response String -> Either String [Lesson T.Text]
toLesson n (Response { rspBody = r }) = maybe (Left "No parse") (Right . join . map handleSubject) lessons
  where
    table      = find (grabTableRegex n) (T.pack r) >>= group 1
    rawLessons = snd <$> ((mapMaybe (group 1) . findAll trRegex <$> table) >>= uncons)
    lessons    = map (mapMaybe (group 1) . findAll tdRegex) <$> rawLessons


scrapeTuDresden :: Int -> IO (Either String [Lesson T.Text])
scrapeTuDresden n = either (Left . show) (toLesson n) <$> retry stdRetries getPage
