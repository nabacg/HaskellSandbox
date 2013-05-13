import Control.Concurrent
import Control.Monad
import System.IO
import Network.Curl.Download
-- import qualified Data.ByteString as B

main2 = do
    hSetBuffering stdout NoBuffering
    forkIO (forever (putChar '_'))
    forkIO (forever (putChar '-'))
    threadDelay (10^6)


main3 = do
    m <- newEmptyMVar
    forkIO $ putMVar m 'a'
    str <- takeMVar m
    putStrLn $ [str]

main4 = do
    m <- newEmptyMVar
    forkIO (takeMVar m >>= print)
    putMVar m 'a'

main5 = do
    m <- newEmptyMVar
    forkIO (do putMVar m 'a'; putMVar m 'b')
    --result <- sequence [takeMVar m, takeMVar m]
    --sequence (map print result)
    takeMVar m >>= print
    takeMVar m >>= print
numThreads = 1000000

mainMillionThreads = do
    m <- newEmptyMVar
    replicateM_ numThreads $ forkIO (putMVar m ())
    replicateM_ numThreads $ takeMVar m

main6 = do
    ms <- replicateM numThreads $ do
        m <- newEmptyMVar
        forkIO (putMVAR m ())
        return m
    mapM_ takeMVar ms

main7 = do
    m1 <- newEmptyMVar
    m2 <- newEmptyMVar
   
    forkIO $ do
        r <- openURI "http://en.wikipedia.org/wiki/Freetown_Christiania"
        case r of
            Left err -> print err
            Right doc -> putMVar m1 doc
   
    forkIO $ do
        r <- openURI "http://en.wikipedia.org/wiki/Anarchism"
        case r of
            Left err -> print err
            Right doc -> putMVar m2 doc

    r1 <- takeMVar m1
    r2 <- takeMVar m2
    return (r1, r2)    