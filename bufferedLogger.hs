import Control.Concurrent

type Stream a = MVar (Item a)

data Item a = Item a (Stream a)
data Channel a = Channel (MVar (Stream a)) (MVar (Stream a))


newChan :: IO (Channel a)
newChan = do
  hole <- newEmptyMVar
  readVar <- newMVar hole
  writeVar <- newMVar hole
  return (Channel readVar writeVar)


writeChan :: Channel a -> a -> IO ()
writeChan (Channel _ writeVar) val = do
  newHole <- newEmptyMVar
  oldHole <- takeMVar writeVar
  putMVar writeVar newHole
  putMVar oldHole (Item val newHole)

readChan :: Channel a -> IO a
readChan (Channel readVar _) = do
  oldHole <- takeMVar readVar
  Item val next <- takeMVar oldHole
  putMVar readVar next
  return val
  

