import Control.Concurrent
import Control.Monad

data Logger = Logger (MVar LogCommand)
data LogCommand = Message String | Stop (MVar ())

initLogger :: IO Logger
initLogger = do
	m <- newEmptyMVar
	let l = Logger m
	forkIO (loggerThread l)
	return l


loggerThread :: Logger -> IO ()
loggerThread (Logger m) = forever $ do
	cmd <- takeMVar m
	case cmd of 
		Message msg -> do
			threadDelay 1000000
			putStrLn msg
		Stop s -> do
			putStrLn "logger: stop"
			putMVar s ()
							


logMessage :: Logger -> String -> IO ()
logMessage (Logger m) msg = putMVar m (Message msg)

logStop :: Logger -> IO ()
logStop (Logger m ) = do
	s <- newEmptyMVar
	putMVar m (Stop s)
	takeMVar s

main = do
	l <- initLogger
	logMessage l "Init"
	logMessage l "Starting"
	logMessage l "Started!"
	logStop l