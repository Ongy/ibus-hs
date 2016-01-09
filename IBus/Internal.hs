module IBus.Internal
where

import System.Environment.XDG.BaseDir (getUserConfigDir)
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)


getIBusLocalId :: IO String
getIBusLocalId = do
  var <- doesFileExist "/var/lib/dbus/machine-id"
  if var
    then init <$> readFile "/var/lib/dbus/machine-id"
    else do
      etc <- doesFileExist "/etc/machine-id"
      if etc
        then init <$> readFile "/etc/machine-id"
        else return "machine-id"


getIBusSocketPath :: IO String
getIBusSocketPath = do
  path <- lookupEnv "IBUS_ADDRESS_FILE"
  case path of
    Just x -> return x
    Nothing -> getPath
  where
    getPath :: IO String
    getPath = do
      disp <- fromMaybe ":0.0" <$> lookupEnv "DISPLAY"
      ibusId <- getIBusLocalId
      let hn = takeWhile (/= ':') disp
      let host = if hn == [] then "unix" else hn
      let dpn = takeWhile (/= '.') . tail . dropWhile (/= ':') $disp
      let fname = ibusId ++ ('-':host) ++ ('-':dpn)
      cfgD <- getUserConfigDir "ibus/bus/"
      return (cfgD ++ fname)


getIBusAddress :: IO String
getIBusAddress = do
  env <- lookupEnv "IBUS_ADDRESS"
  case env of
    Just x -> return x
    Nothing -> do
      content <- fmap lines . readFile =<< getIBusSocketPath
      let file = tail . dropWhile (/= '=') .  head . filter ("IBUS_ADDRESS=" `isPrefixOf`) $content
      --pid <- dropWhile (/= '=') <$> head . filter ("IBUS_DAEMON_PID=" `isPrefixPof`) $content
      return file

