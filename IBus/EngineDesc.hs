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
{-# LANGUAGE OverloadedStrings #-}

module IBus.EngineDesc
  ( IBusEngineDesc (..)
  , getIBusEngine
  , subscribeToEngine
  )
where

import Control.Monad (join)
import Data.Maybe (fromJust)
import Data.Word (Word32)
import DBus
import DBus.Client
import IBus

type SignalH = SignalHandler

-- |The Datatype for an IBus Engine description
data IBusEngineDesc
  = IBusEngineDesc
  { engineName :: String
  , engineLongName :: String
  , engineDescription :: String
  , engineLanguage :: String
  , engineLicense :: String
  , engineAuthor :: String
  , engineIcon :: String
  , engineLayout :: String
  , engineRank :: Int
  , engineHotkeys :: String
  , engineSymbol :: String
  , engineSetup :: String
  , engineLayoutVar :: String
  , engineLayoutOpt :: String
  , engineVersion :: String
  , engineDomain :: String
  , engineIconKey :: String
  } deriving (Eq, Show)

getBody :: MethodReturn -> Variant
getBody = fromJust . join . fmap fromVariant . fromVariant . head . methodReturnBody

replyToEngineDesc :: MethodReturn -> Maybe IBusEngineDesc
replyToEngineDesc m = do
  let [name, _, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17] = structureItems . fromJust . fromVariant . getBody $m
  a1 <- fromVariant v1 :: Maybe String
  a2 <- fromVariant v2 :: Maybe String
  a3 <- fromVariant v3 :: Maybe String
  a4 <- fromVariant v4 :: Maybe String
  a5 <- fromVariant v5 :: Maybe String
  a6 <- fromVariant v6 :: Maybe String
  a7 <- fromVariant v7 :: Maybe String
  a8 <- fromVariant v8 :: Maybe String
  a9 <- fromVariant v9 :: Maybe Word32
  a10 <- fromVariant v10 :: Maybe String
  a11 <- fromVariant v11 :: Maybe String
  a12 <- fromVariant v12 :: Maybe String
  a13 <- fromVariant v13 :: Maybe String
  a14 <- fromVariant v14 :: Maybe String
  a15 <- fromVariant v15 :: Maybe String
  a16 <- fromVariant v16 :: Maybe String
  a17 <- fromVariant v17 :: Maybe String
  if (fromJust $fromVariant name :: String) == "IBusEngineDesc"
    then return (IBusEngineDesc a1 a2 a3 a4 a5 a6 a7 a8 (fromIntegral a9) a10 a11 a12 a13 a14 a15 a16 a17)
    else Nothing

-- |Get the Global IBus engine from the bus
getIBusEngine :: IBusClient -> IO IBusEngineDesc
getIBusEngine client = fmap (fromJust . replyToEngineDesc) $
  call_ client (methodCall "/org/freedesktop/IBus" "org.freedesktop.DBus.Properties" "Get")
        { methodCallDestination = Just "org.freedesktop.IBus"
        , methodCallBody = 
          [ toVariant ("org.freedesktop.IBus" :: String)
          , toVariant ("GlobalEngine" :: String)
          ]
        }


getNewOrder :: Signal -> Maybe [String]
getNewOrder s = do
  [section, option] <- sequence . map fromVariant . init . signalBody $s:: Maybe [String]
  vars' <- fromVariant . last . signalBody $s
  vars <- fromVariant vars' :: Maybe [String]
  if section == "general" && option == "engines_order"
    then return vars
    else Nothing


subscribeToEngine :: Client -> ([String] -> IO ()) -> IO SignalH
subscribeToEngine client f =
  addMatch client
    (matchAny {matchPath = Just "/org/freedesktop/IBus/Config"})
    (\s -> case getNewOrder s of
      Just x -> f x
      Nothing -> return ())
