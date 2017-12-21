import Text.CSV
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.LocalTime
import Data.Time.Calendar.WeekDate
import Data.List.Split

-- Parses a CSV file
main :: IO ()
main = do
  now <- getCurrentTime
  print now
  print (diffDays (getDay "2017-12-2") (utctDay now))
  print (toInt "01")
  let fileName = "notebook.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  either handleError doWork csv now
  
handleError csv now = putStrLn "not a CSV"
doWork csv now = --print ((findFirstLetterName.tail) csv 'A') 
                 --print ((noMeetings.tail) csv)
                 print (closestMeeting csv now) 

noMeetings :: [Record] -> [Record]
noMeetings [] = []
noMeetings items = filter (\ i -> wasmeeting i == "no") items 

closestMeeting :: [Record] -> UTCTime -> Record
closestMeeting items now = foldl1 (\a x -> 
  if (diffDays (getDay (daymeeting x)) (utctDay now)) < 
     (diffDays (getDay (daymeeting a)) (utctDay now)) then x else a) (noMeetings items)

findFirstLetterName :: [Record] -> Char -> [String]
findFirstLetterName items s = map (\ e -> (name e)) (filter (\ e -> (name e)!!0 == s) items)

wasmeeting [_,_,_,_,_,a] = wasMeeting a
name [a,_,_,_,_,_] = getName a
daymeeting [_,_,_,_,a,_] = dayMeeting a

getName :: String -> String
getName item = item 

wasMeeting :: String -> String
wasMeeting item = item 

dayMeeting :: String -> String
dayMeeting item = item 

getDatesMeeting :: [Record] -> [String]
getDatesMeeting items = map (\ i -> daymeeting i) items

getDay :: String -> Day
getDay date = fromGregorian (returnYear (toYear date) 0) 
                            (returnDM (toDate date) 1) 
                            (returnDM (toDate date) 2)

toDate :: String -> [Int]
toDate str = map (\i -> toInt i) (splitOn "-" str)

toYear :: String -> [Integer]
toYear str = map (\i -> toInteger' i) (splitOn "-" str)

returnYear :: [Integer] -> Int -> Integer
returnYear arr index = arr !! index

returnDM :: [a] -> Int -> a
returnDM arr index = arr !! index

toInt :: String -> Int                              
toInt = read

toInteger' :: String -> Integer                              
toInteger' = read
