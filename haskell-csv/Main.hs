import Text.CSV

-- Parses a CSV file
main :: IO ()
main = do
  let fileName = "notebook.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  either handleError doWork csv
  
handleError csv = putStrLn "not a CSV"
doWork csv =  print ((findFirstLetterName.tail) csv 'A') 
       --csv = print ((noMeetings.tail) csv)
       
noMeetings :: [Record] -> [Record]
noMeetings [] = []
noMeetings items = filter (\ i -> wasmeeting i == "no") items 

findFirstLetterName :: [Record] -> Char -> [String]
findFirstLetterName items s = map (\ e -> (name e)) (filter (\ e -> (name e)!!0 == s) items)

wasmeeting [_,_,_,_,_,a] = wasMeeting a
name [a,_,_,_,_,_] = getName a

getName :: String -> String
getName item = item 

wasMeeting :: String -> String
wasMeeting item = item 
