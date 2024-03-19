module Renderer.Helper where

import Graphics.Svg
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

testPath :: String -> FilePath
testPath context = "/Users/felixvalentini/dev/smapr/test/" ++ context  ++ ".svg":: FilePath

dateTimeStr :: IO String
dateTimeStr = getCurrentTime >>=
  \currentTime -> return $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" currentTime


writeSvg :: Element -> IO ()
writeSvg svg = do
  dateString <- dateTimeStr
  let destPath = testPath dateString
  putStrLn destPath
  renderToFile destPath svg
