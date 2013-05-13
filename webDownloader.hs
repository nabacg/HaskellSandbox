import Control.Monad
import Control.Concurrent
import Network.Curl

getURL :: String -> IO String
getURL url = do
  (code, content) <-curlGetString url []
  return content

main = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
   
    forkIO $ do
        r <- getURL "http://en.wikipedia.org/wiki/Freetown_Christiania"
        putMVar m1 r
        
    forkIO $ do
        r <- getURL "http://en.wikipedia.org/wiki/Anarchism"
        putMVar m2 r
        
    r1 <- takeMVar m1
    r2 <- takeMVar m2
    return (r1, r2)    