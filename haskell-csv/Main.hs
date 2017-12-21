import Text.CSV

-- Parses a CSV file
main :: IO ()
main = do
  let fileName = "notebook.csv"
  input <- readFile fileName
  let csv = parseCSV fileName input
  either handleError doWork csv
  
handleError csv = putStrLn "not a CSV"
doWork csv = print ((findFirstLetterName.tail) csv 'A') 
               
findFirstLetterName :: [Record] -> Char -> [String]
findFirstLetterName items s = map (\ e -> (name e)) (filter (\ e -> (name e)!!0 == s) items)

name [a,_,_,_,_,_] = getName a

getName :: String -> String
getName item = item 

data Book = Book { bookname :: String, author :: String, city :: String, house :: String, year :: Integer  } deriving (Show) 

books :: [Book]
books = [Book {bookname = "Book1", author = "Author1", city = "City1", house = "House1", year = 2017},
         Book {bookname = "Book2", author = "Author2", city = "City2", house = "House2", year = 2016}];

getBooksByAuthor :: [Book] -> String -> [Book]
getBooksByAuthor books a = filter (\ book -> if (book author == a) then True else False) books

getBookName :: (Book -> String) -> String
getBookName a = show a