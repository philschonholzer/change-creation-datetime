import Data.Bifunctor (Bifunctor (first))
import Data.Function ((&))
import Data.Time
import System.Environment (getArgs)
import System.Process (callCommand, readProcess)

main :: IO ()
main = do
  args <- getArgs
  let offset = args & head & read & realToFrac :: NominalDiffTime
  creationdates <- lines <$> readProcess "stat" ("-c \"%w\"" : tail args) ""
  let creationdatesAndFiles = args & tail & zip creationdates
  mapM_ (updateFile . first (addOffsetToDate offset)) creationdatesAndFiles
  putStrLn "Changed creation date."

cet :: TimeZone
cet = hoursToTimeZone 1

addOffsetToDate :: NominalDiffTime -> [Char] -> [Char]
addOffsetToDate offset =
  show
    . formatToUs
    . utcToZonedTime cet
    . addUTCTime offset
    . parseUnixTime
    . removeQuotations

usFormat :: [Char]
usFormat = "%m/%d/%Y %T"

formatToUs :: ZonedTime -> String
formatToUs = formatTime defaultTimeLocale usFormat

unixTimeFormat :: [Char]
unixTimeFormat = "%Y-%-m-%-d %H:%M:%S%Q %z"

parseUnixTime :: String -> UTCTime
parseUnixTime = parseTimeOrError True defaultTimeLocale unixTimeFormat

removeQuotations :: [Char] -> [Char]
removeQuotations = filter (`notElem` "\"")

updateFile :: ([Char], [Char]) -> IO ()
updateFile (date, file) = callCommand ("SetFile -d " ++ date ++ " " ++ file)
