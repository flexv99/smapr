module Renderer.Helper where

import Graphics.Svg
import Data.Time.Clock (getCurrentTime, UTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

testPath :: String -> FilePath
testPath context = "/Users/felixvalentini/personal_site/smapr/test/" ++ context  ++ ".svg":: FilePath

dateTimeStr :: IO String
dateTimeStr = getCurrentTime >>= \currentTime -> return $ formatTime defaultTimeLocale "%Y-%m-%d_%H:%M:%S" currentTime


writeSvg :: Element -> IO ()
writeSvg svg = dateTimeStr >>= \d -> renderToFile (testPath d) svg
