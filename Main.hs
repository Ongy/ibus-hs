{-# LANGUAGE OverloadedStrings #-}

import IBus
import IBus.EngineDesc
import Control.Concurrent (threadDelay)


main :: IO ()
main = do
  client <- iBusConnect

  _ <- subscribeToEngine client (putStrLn . show . head)
  
  engine <- getIBusEngine client
  putStrLn (engineName engine)
  threadDelay (10 * 1000 * 1000)
