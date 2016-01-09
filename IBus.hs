module IBus
where

import Data.Maybe (fromJust)

import DBus (parseAddress)
import DBus.Client (connect, Client)

import IBus.Internal

type IBusClient = Client

iBusConnect :: IO IBusClient
iBusConnect = connect . fromJust . parseAddress =<< getIBusAddress
