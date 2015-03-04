{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import qualified Data.ByteString.Char8       as B (pack)
import           Data.List
import           Data.Maybe
import           Data.Text                   (Text)
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Network.HTTP.Conduit
import           Options.Applicative
import           Text.XML
import           Text.XML.Lens


-- |
-- = OData collections

share
  [ mkPersist sqlSettings { mpsPrefixFields   = True
                          , mpsGeneric        = False
                          , mpsGenerateLenses = True
                          }
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
    DivisioTerritorial
      -- We can use enumerated types in persistent, but they require a more
      -- involved use of TH that breaks Emacs flycheck. They are translated into
      -- SQL varchars anyway.
      -- categoria_divisio CategoriaDivisio
      codi_divisio_territorial          Text           sqltype=varchar(4)
      nom_divisio_territorial           Text
      categoria_divisio                 Text
      codi_divisio_territorial_pare     Text Maybe     sqltype=varchar(4)
      url_fitxa_divisio_territorial     Text Maybe

      Primary codi_divisio_territorial
      Foreign DivisioTerritorial fkey codi_divisio_territorial_pare
      UniqueUrlFitxaDivisioTerritorial url_fitxa_divisio_territorial !force
      deriving Show
  |]

divisioTerritorial :: ReifiedFold Element DivisioTerritorial
divisioTerritorial = DivisioTerritorial
  <$> Fold ( propText "codi_divisio_territorial"       . to fromJust )
  <*> Fold ( propText "nom_divisio_territorial"        . to fromJust )
  <*> Fold ( propText "categoria_divisio"              . to fromJust )
  <*> Fold ( propText "codi_divisio_territorial_pare"                )
  <*> Fold ( propText "url_fitxa_divisio_territorial"                )


-- |
-- = Command line options

--data Options = Options { command :: Command }

--data Command = List | Get GetOptions

-- data GetOptions
--   = GetOptions
data Options
  = Options
    { dbname   :: String
    , user     :: String
    , password :: String
    }

options :: Parser Options
options = Options
  <$> option (str >>= param "dbname")
        ( long "dbname"
          <> short 'd'
          <> metavar "DB"
          <> help "Passes parameter dbname=DB to database connection"
          <> value ""
        )
  <*> option (str >>= param "user")
        ( long "username"
          <> short 'u'
          <> metavar "USER"
          <> help "Passes parameter user=USER to database connection"
          <> value ""
        )
  <*> option (str >>= param "password")
        ( long "password"
          <> short 'p'
          <> metavar "PASSWD"
          <> help "Passes param. password=PASSWD to database connection"
          <> value ""
        )
  where
    param _ "" = return ""
    param p s  = return $ p ++ "=" ++ s ++ " "

helpMessage :: InfoMod a
helpMessage =
  fullDesc
  <> progDesc "Connect to database and do things"
  <> header "bcnodata2db - Relational-ize OData from http://opendata.bcn.cat"

-- |
-- = Entry point and auxiliary functions

main :: IO ()
main = execParser options' >>= \(Options d u p) -> do
  document <- readDocument "OPENDATADIVTER0.xml"
  let entryList = document ^.. entries . runFold divisioTerritorial
  -- mapM_ print entryList
  print $ length entryList
  runStderrLoggingT $ withPostgresqlPool (pqConnOpts d u p) 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      insertMany $ sortBy districtesPrimer entryList
  return ()
    where
      districtesPrimer (DivisioTerritorial _ _ _ Nothing _) _ = LT
      districtesPrimer _ (DivisioTerritorial _ _ _ Nothing _) = GT
      districtesPrimer _ _                                    = EQ
      options' = info (helper <*> options) helpMessage
      pqConnOpts d u p = B.pack $ concat [d, u, p]

-- | Traversal of an OData resource (an XML Document) focusing on all entries
-- (i.e. database rows).
entries :: Traversal' Document Element
entries =
     root
  .  named "feed"
  ./ named "entry"
  ./ named "content"
  ./ named "properties"

-- | Fold on XML Element's representing the properties (an XML Element called
-- "properties") of an OData entity. Given a property name, it returns the Fold
-- for the text associated to this property, if it exists in the Element
-- (Nothing if not).
propText :: Text -> Fold Element (Maybe Text)
propText propName = to (findOf elemAndText hasPropName) . to (fmap snd)
  where
    -- Remember that l ./ m == l . plate . m
    hasPropName (e, _) = e ^. localName == propName
    elemAndText :: Fold Element (Element, Text)
    elemAndText = plate . runFold ((,) <$> Fold (to id) <*> Fold text)

readCollectionList :: IO [Text]
readCollectionList = do
  document <- readDocument ""
  return $ document ^.. titles

-- | Traversal of an OData collection catalog (an XML Document) focusing on all
-- collection titles.
titles :: Traversal' Document Text
titles =
     root
  .  named "service"
  ./ named "workspace"
  ./ named "collection"
  ./ named "title"
  .  text

odataBaseUrl :: String
odataBaseUrl = "http://bcnodata.cloudapp.net:8080/v1/Data/"

readDocument :: String -> IO Document
readDocument docName = do
  result <- simpleHttp $ odataBaseUrl ++ docName
  return $ parseLBS_ def result
