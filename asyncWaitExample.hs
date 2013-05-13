import Control.Concurrent
import Network.Curl
import Control.Exception

newtype Async a = Async (MVar (Either SomeException a))

getURL :: String -> IO String
getURL url = do
  (code, content) <-curlGetString url []
  return content

async :: IO a -> IO (Async a)
async io = do
  m <- newEmptyMVar
  forkIO $ do r <- try io; putMVar m r
  return (Async m)


wait :: Async a -> IO a
wait (Async m) = do
  e <- readMVar m
  case e of
    Left e -> throwIO e
    Right a -> return a

waitCatch :: Async a -> IO (Either SomeException a)
waitCatch (Async m) = readMVar m

sites = ["http://www.google.com", "http://www.bing.com",
         "http://www.haskell.org", "http://www.bankier.pl"]

main = do
  a1 <- async $ getURL "http://en.wikipedia.org/wiki/Freetown_Christiania"
  a2 <- async $ getURL "http://en.wikipedia.org/wiki/Anarchism"
  r1 <- wait a1
  r2 <- wait a2

  as <- mapM ( async . getURL ) sites
  rs <- mapM wait as
  mapM print rs
  
  return (r1, r2)