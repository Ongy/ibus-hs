{-
    Copyright 2016 Markus Ongyerth

    This file is part of ibus-hs.

    Monky is free software: you can redistribute it and/or modify
    it under the terms of the GNU Lesser General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    Monky is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public License
    along with Monky.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE CPP #-}
module IBus.Internal
where


#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

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
      -- TODO add the check for running process the original library does
      --pid <- dropWhile (/= '=') <$> head . filter ("IBUS_DAEMON_PID=" `isPrefixPof`) $content
      return file

