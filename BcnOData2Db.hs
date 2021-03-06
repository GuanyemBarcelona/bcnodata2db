-- Copyright (C) 2015  Guillem Marpons

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import           BcnOData.Persistent
import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class      (MonadIO, liftIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import qualified Data.ByteString.Char8       as B8 (pack)
import           Data.Char
import qualified Data.Map.Strict             as M
import           Data.Maybe
import qualified Data.Set                    as S
import           Data.String
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Data.Time.LocalTime
import           Database.Persist
import           Database.Persist.Postgresql
import           Network                     (withSocketsDo)
import           Network.HTTP.Conduit
import           Options.Applicative
import           Text.XML
import           Text.XML.Lens


-- |
-- = OData collection folds

data MetaInfo
  = MetaInfo { origen        :: Text -- Institution that generated the data set
             , transformacio :: Text
             , url1          :: Text
             , url2          :: Text
             }

data AnyCollection
  = forall r. (PersistEntity r, PersistEntityBackend r ~ SqlBackend)
    =>
    MkAC (ReifiedFold Element r) -- Reified fold for getting resources
         [r]                     -- List of manually inserted resources
         [Filter r]              -- Filter to select resources to delete

collections ::
  [
    ( String                    -- URL suffix
    , MetaInfo
    , Maybe Text                -- Property that needs to be present
    , Maybe Text                -- Value that preceding property needs to take
    , AnyCollection             -- Means to get all resources of a collection
    )
  ]
collections =
  [
    (
      "OPENDATADIVTER0",
      MetaInfo "" ""
        "http://opendata.bcn.cat/opendata/ca/catalog/SECTOR_PUBLIC/tauladivadmnou/"
        "http://w20.bcn.cat/cartobcn/",
      Just "categoria_divisio", Just "Districte",
      MkAC (
        Districtes
        <$> Fold (   propText "codi_divisio_territorial"
                   . to (fmap (T.drop 2))
                   . toRead
                   . to fromJust                                     )
        <*> Fold ( propText "nom_divisio_territorial"  . to fromJust )
        )
      [ Districtes 0 "DISTRICTE NO ASSIGNAT" ] []
    )
  , (
      "OPENDATADIVTER0",
      MetaInfo "" ""
        "http://opendata.bcn.cat/opendata/ca/catalog/SECTOR_PUBLIC/tauladivadmnou/"
        "http://w20.bcn.cat/cartobcn/",
      Just "categoria_divisio", Just "Barri",
      MkAC (
        Barris
        <$> Fold (   propText "codi_divisio_territorial"
                   . to (fmap (T.drop 2))
                   . toRead
                   . to fromJust                                       )
        <*> Fold (   propText "nom_divisio_territorial"  . to fromJust )
        <*> Fold (   propText "codi_divisio_territorial_pare"
                   . to (fmap (T.drop 2))
                   . toRead                                            )
        <*> Fold ( propText "url_fitxa_divisio_territorial"            )
        )
      [ Barris 0 "BARRI NO ASSIGNAT" Nothing Nothing ] []
    )
  , ( "OPENDATAIMMIGRACIOSEXE2013",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/immigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/imi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                              )
        <*> Fold (   to (const 2013)                          )
        <*> Fold (   to (const "I")                           )
        <*> Fold (   propText "dte"    . toRead               )
        <*> Fold (   propText "dones"  . toRead . to fromJust )
        <*> Fold (   propText "homes"  . toRead . to fromJust )
        <*> Fold (   propText "total"  . toRead . to fromJust )
        )
      [] []
    )
  , ( "OPENDATAIMMIGRACIOSEXE2012",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/immigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/imi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                              )
        <*> Fold (   to (const 2012)                          )
        <*> Fold (   to (const "I")                           )
        <*> Fold (   propText "dte"    . toRead               )
        <*> Fold (   propText "dones"  . toRead . to fromJust )
        <*> Fold (   propText "homes"  . toRead . to fromJust )
        <*> Fold (   propText "total"  . toRead . to fromJust )
        )
      [] []
    )
  , ( "immigraciosexe2011",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/immigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/imi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2011)                                      )
        <*> Fold (   to (const "I")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "immigraciosexe2010",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/immigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/imi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2010)                                      )
        <*> Fold (   to (const "I")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "immigraciosexe2009",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/immigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/imi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2009)                                      )
        <*> Fold (   to (const "I")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "immigraciosexe2008",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/immigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/imi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2008)                                      )
        <*> Fold (   to (const "I")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "immigraciosexe2007",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/immigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/imi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2007)                                      )
        <*> Fold (   to (const "I")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "OPENDATAEMIGRACIOSEXE2013",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/emigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/emi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                              )
        <*> Fold (   to (const 2013)                          )
        <*> Fold (   to (const "E")                           )
        <*> Fold (   propText "dte"    . toRead               )
        <*> Fold (   propText "dones"  . toRead . to fromJust )
        <*> Fold (   propText "homes"  . toRead . to fromJust )
        <*> Fold (   propText "total"  . toRead . to fromJust )
        )
      [] []
    )
  , ( "OPENDATAEMIGRACIOSEXE2012",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/emigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/emi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                              )
        <*> Fold (   to (const 2012)                          )
        <*> Fold (   to (const "E")                           )
        <*> Fold (   propText "dte"    . toRead               )
        <*> Fold (   propText "dones"  . toRead . to fromJust )
        <*> Fold (   propText "homes"  . toRead . to fromJust )
        <*> Fold (   propText "total"  . toRead . to fromJust )
        )
      [] []
    )
  , ( "emigraciosexe2011",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/emigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/emi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2011)                                      )
        <*> Fold (   to (const "E")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "emigraciosexe2010",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/emigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/emi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2010)                                      )
        <*> Fold (   to (const "E")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "emigraciosexe2009",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/emigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/emi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2009)                                      )
        <*> Fold (   to (const "E")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "emigraciosexe2008",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/emigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/emi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2008)                                      )
        <*> Fold (   to (const "E")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  , ( "emigraciosexe2007",
      MetaInfo
        "Departament d'Estadística. Ajuntament de Barcelona"
        ""
        "http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/emigracio-sexe/"
        "http://www.bcn.es/estadistica/catala/dades/barris/tdemo/emi/index.htm",
      Just "barris", Nothing,
      MkAC (
        MigrantsPerSexe
        <$> Fold (   propText "barris"
                   . to handleBarriNoConsta
                   . toTakeNumPrefix
                   . toRead
                   . to fromJust                                          )
        <*> Fold (   to (const 2007)                                      )
        <*> Fold (   to (const "E")                                       )
        <*> Fold (   propText "dte"   . toReadNumWithPeriod               )
        <*> Fold (   propText "dones" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "homes" . toReadNumWithPeriod . to fromJust )
        <*> Fold (   propText "total" . toReadNumWithPeriod . to fromJust )
        )
      [] []
    )
  ]

  -- More recent data about unemployment evolution can be found in url2, in
  -- Excel format.

  ++
  ( nomsMesos <&>
    (\(nomMes, numMes) ->
      ( "OPENDATAATUREVO2013",
        MetaInfo
          "Departament d'Empresa i Ocupació. Generalitat de Catalunya"
          "Estimació"
          "http://opendata.bcn.cat/opendata/ca/catalog/OCUPACIO/aturevo/"
          "http://www.bcn.cat/estadistica/catala/dades/barris/ttreball/atur/evolucio/index.htm",
        Just "gener", Nothing,
        MkAC (
          AturatsEvolucioMensual
          <$> Fold (   propText "barris"
                     . to handleBarriNoConsta
                     . toTakeNumPrefix
                     . toRead
                     . to fromJust                                          )
          <*> Fold (   to (const 2013)                                      )
          <*> Fold (   to (const numMes)                                    )
          <*> Fold (   propText "dte"   . toRead                            )
          <*> Fold (   propText nomMes  . toReadNumWithPeriod . to fromJust )
          )
        [] []
      )
    )
  )

  ++
  ( nomsMesos <&>
    (\(nomMes, numMes) ->
      ( "OPENDATAATUREVO2012",
        MetaInfo
          "Departament d'Empresa i Ocupació. Generalitat de Catalunya"
          "Estimació"
          "http://opendata.bcn.cat/opendata/ca/catalog/OCUPACIO/aturevo/"
          "http://www.bcn.cat/estadistica/catala/dades/barris/t/atur/evolucio/index.htm",
        Just "gener", Nothing,
        MkAC (
          AturatsEvolucioMensual
          <$> Fold (   propText "barris"
                     . to handleBarriNoConsta
                     . toTakeNumPrefix
                     . toRead
                     . to fromJust                                          )
          <*> Fold (   to (const 2012)                                      )
          <*> Fold (   to (const numMes)                                    )
          <*> Fold (   propText "dte"   . toRead                            )
          <*> Fold (   propText nomMes  . toReadNumWithPeriod . to fromJust )
          )
        [] []
      )
    )
  )

  ++
  ( nomsMesos <&>
    (\(nomMes, numMes) ->
      ( "OPENDATAATUREVO2011",
        MetaInfo
          "Departament d'Empresa i Ocupació. Generalitat de Catalunya"
          "Estimació"
          "http://opendata.bcn.cat/opendata/ca/catalog/OCUPACIO/aturevo/"
          "http://www.bcn.cat/estadistica/catala/dades/barris/ttreball/atur/evolucio/index.htm",
        Just "gener", Nothing,
        MkAC (
          AturatsEvolucioMensual
          <$> Fold (   propText "barris"
                     . to handleBarriNoConsta
                     . toTakeNumPrefix
                     . toRead
                     . to fromJust                                          )
          <*> Fold (   to (const 2011)                                      )
          <*> Fold (   to (const numMes)                                    )
          <*> Fold (   propText "dte"   . toRead                            )
          <*> Fold (   propText nomMes  . toReadNumWithPeriod . to fromJust )
          )
        [] []
      )
    )
  )

nomsMesos :: [(Text, Int)]
nomsMesos = [ ("gener", 1), ("febrer", 2), ("març", 3)
            , ("abril", 4), ("maig", 5), ("juny", 6)
            , ("juliol", 7), ("agost", 8), ("setembre", 9)
            , ("octubre", 10), ("novembre", 11), ("desembre", 12) ]

toRead :: (Read a, Functor f) => Fold (f Text) (f a)
toRead = to (fmap T.unpack) . to (fmap read)

toReadNumWithPeriod :: (Functor f) => Fold (f Text) (f Int)
toReadNumWithPeriod =   to (fmap T.unpack)
                      . to (fmap (filter isNumber))
                      . to (fmap read)

toTakeNumPrefix :: Functor f => Fold (f Text) (f Text)
toTakeNumPrefix = to $ fmap (T.takeWhile isNumber)

handleBarriNoConsta :: (Data.String.IsString a, Eq a) => Maybe a -> Maybe a
handleBarriNoConsta (Just "No consta") = Just "0"
handleBarriNoConsta b                  = b


-- |
-- = Command line options

data Options
  = Options
    { dbname             :: String
    , user               :: String
    , password           :: String
    , usersToGrantAccess :: [String]
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
  <*> option (liftM (fmap T.unpack . T.splitOn "," . T.pack) str)
  ( long "grant-access-to"
    <> short 'g'
    <> metavar "USERS"
    <> help "Comma-separated list of DB users to grant access privileges"
    <> value []
  )
  where
    param _ "" = return ""
    param p s  = return $ p ++ "=" ++ s ++ " "

helpMessage :: InfoMod a
helpMessage =
  fullDesc
  <> progDesc "Connect to database and do things"
  <> header "bcnodata2db - Relational-ize OData from http://opendata.bcn.cat"


-- | = Entry point and auxiliary functions.

-- | Entry point. All SQL commands are run in a single transaction. This has
-- some advantages (performance, if something fails no data is destroyed) and
-- one big drawback: if a collection gets broken, we can't update the other
-- collections.

-- To change the aforementioned behavior see
-- Database.Persistent.SQLtransactionSave
main :: IO ()
main = execParser options' >>= \(Options d u p g) -> handle non2xxStatusExp $ do
    runNoLoggingT $ withPostgresqlPool (pqConnOpts d u p) 10 $ \pool ->
      liftIO $ flip runSqlPersistMPool pool $ do
        runMigration migrateAll
        forM_ g $ \user_ ->
          handleAll (expWhen ("granting access privileges for user " ++ user_)) $
            grantAccess "fonts_de_dades" user_
        -- Delete (usually all) table rows before inserting.
        -- Delete in reverse order due to foreign key dependencies.
        liftIO $ putStrLn "Deleting rows"
        mapM_ deleteAll $ reverse collections
        mapM_ (insertAll g) collections
    return ()
  where
    -- insertAll also updates meta-info tables and access privileges for users
    -- in `g'
    insertAll :: (MonadBaseControl IO m, MonadLogger m, MonadIO m, MonadCatch m)
                 =>
                 [String]
              -> (String, MetaInfo, Maybe Text, Maybe Text, AnyCollection)
              -> ReaderT SqlBackend m ()
    insertAll users (docName, meta, prop, val, MkAC fold moreResources _) = do
      let font_ = odataBaseUrl ++ docName
      doc <- liftIO $ readDocument font_
      let resources = doc ^.. filteringTraversal prop val . runFold fold
      -- Works even with empty list!!
      let taula_ = T.filter (/='"') $ tableName $ head resources
      liftIO $ putStr "Inserting to " >> putStr (show taula_) >> putStr " "
      handleAll (expWhen "inserting resources") $ do
        keys <- insertMany $ resources ++ moreResources
        liftIO $ putStr (show (length keys)) >> putStrLn " rows"
      let MetaInfo o t u1 u2 = meta
      let taula_i_font_ = T.concat [taula_, " <= ", T.pack font_]
      zonedTime <- liftIO getZonedTime
      let now = zonedTimeToUTC zonedTime
      handleAll (expWhen "inserting meta info data") $ do
        _ <- upsert (FontsDeDades taula_i_font_ "OData-XML" now o t u1 u2) []
        return ()
      forM_ users $ \user_ ->
        handleAll (expWhen ("granting access privileges for user " ++ user_)) $
          grantAccess taula_ user_
    deleteAll :: (MonadBaseControl IO m, MonadLogger m, MonadIO m, MonadCatch m)
                 =>
                 (String, MetaInfo, Maybe Text, Maybe Text, AnyCollection)
              -> ReaderT SqlBackend m ()
    deleteAll (_, _, _, _, MkAC _ _ deleteFilter) =
      handleAll (expWhen "deleting resources") $ deleteWhere deleteFilter
    filteringTraversal Nothing _         = memberResources
    filteringTraversal (Just p) Nothing  = memberResourcesWithSome p
    filteringTraversal (Just p) (Just v) = memberResourcesWith p v
    options' = info (helper <*> options) helpMessage
    pqConnOpts d u p = B8.pack $ concat [d, u, p]
    non2xxStatusExp :: HttpException -> IO ()
    non2xxStatusExp e = put2Ln >> putStr "Caught " >> print e >> put2Ln
    expWhen :: (MonadIO m, MonadCatch m) => String -> SomeException -> m ()
    expWhen msg e = do
      liftIO $ put2Ln >> putStr "Caught when " >> putStr msg >> putStr " :"
      liftIO $ print e >> put2Ln
    put2Ln = putStrLn "" >> putStrLn ""

grantAccess :: (MonadBaseControl IO m, MonadLogger m, MonadIO m)
               =>
               Text -> String -> ReaderT SqlBackend m ()
grantAccess t u = rawExecute (T.concat ["GRANT SELECT ON ", t," TO ", u']) []
  where
    u' = T.pack u

-- | Traversal of an OData resource collection (an XML Document) focusing on all
-- member resources (i.e. database rows). (Follow terminology found in
-- http://bitworking.org/projects/atom/rfc5023.html#rfc.section.1).
memberResources :: Traversal' Document Element
memberResources =
     root
  .  named "feed"
  ./ named "entry"
  ./ named "content"
  ./ named "properties"
  .  filtered hasNonTrivialProperty
  where
    hasNonTrivialProperty :: Element -> Bool
    hasNonTrivialProperty e = any (`S.notMember` excluded) (subnodes e)
    subnodes :: Element -> [Text]
    subnodes e = e ^.. entire . localName

excluded :: S.Set Text
excluded =
  S.fromList [ "properties", "PartitionKey", "RowKey", "Timestamp", "entityid" ]

-- | Traversal of an OData resource collection (an XML Document) focusing on all
-- member resources (i.e. database rows) that contain the specified
-- property. (Follow terminology found in
-- http://bitworking.org/projects/atom/rfc5023.html#rfc.section.1).
memberResourcesWithSome :: Text -> Traversal' Document Element
memberResourcesWithSome propertyName =
     root
  .  named "feed"
  ./ named "entry"
  ./ named "content"
  ./ named "properties"
  .  filtered hasProperty
  where
    hasProperty e = propertyName `elem` e ^.. entire . localName

memberResourcesWith :: Text -> Text -> Traversal' Document Element
memberResourcesWith propertyName value_ =
     root
  .  named "feed"
  ./ named "entry"
  ./ named "content"
  ./ named "properties"
  .  filtered hasPropWithValue
  where
    hasPropWithValue e = e ^? plate . named propertyName . text == Just value_

-- | Fold on XML Element's representing the properties (an XML Element called
-- "properties") of an OData resource. Given a property name, it returns the
-- Fold for the text associated to this property, if it exists in the Element
-- (Nothing if not).
propText :: Text -> Fold Element (Maybe Text)
propText propName = to (findOf elemAndText hasPropName) . to (fmap snd)
  where
    -- Remember that l ./ m == l . plate . m
    hasPropName (e, _) = e ^. localName == propName
    elemAndText :: Fold Element (Element, Text)
    elemAndText = plate . runFold ((,) <$> Fold (to id) <*> Fold text)

readDocument :: String -> IO Document
readDocument url = withSocketsDo $ do
  result <- simpleHttp url
  return $ parseLBS_ def result

odataBaseUrl :: String
odataBaseUrl = "http://bcnodata.cloudapp.net:8080/v1/Data/"


-- |
-- = service2Persistent

-- | Retrieve all collections in Barcelona ODATA and print information useful
-- for creating a Persistent description of the data.
service2Persistent :: IO ()
service2Persistent = handle readServiceExp $ do
  service <- readDocument ""
  let links = service ^.. collectionLinks
  forM_ links $ \link -> handle (readCollectionExp link) $ do
    let ulink = T.unpack link
    collection <- readDocument ulink
    putStr "    " >> putStrLn ulink
    collection2Persistent collection
    putStrLn ""
    return ()
  where
    readServiceExp :: HttpException -> IO ()
    readServiceExp e = putStr "Caught " >> print e
    readCollectionExp :: Text -> HttpException -> IO ()
    readCollectionExp link e =    putStr "    " >> putStr (T.unpack link)
                               >> putStr " " >> putStr "Caught " >> print e
                               >> putStrLn ""

collection2Persistent :: Document -> IO ()
collection2Persistent collection = do
  let allProperties = collection ^.. memberResources.entire.filtered nonTrivial
  let propertyMap = foldr ins M.empty allProperties
  forM_ (M.toList propertyMap) $ \(propName, (exampleL, num :: Integer)) -> do
    putStr "      " >> putStr (T.unpack propName) >> putStr " "
    putStr (show num) >> putStr " " >> print exampleL
  where
    nonTrivial e = S.notMember (e ^. localName) excluded
    ins e = M.insertWith addExample (e ^. localName) ([e ^. text], 1)
    addExample (newEx, _) (oldEx, 1) = (oldEx ++ newEx, 2)
    addExample _          (oldEx, n) = (oldEx, n + 1)

-- | Traversal of an OData service document (an XML Document) focusing on all
-- collection links. (Follow terminology found in
-- http://bitworking.org/projects/atom/rfc5023.html#rfc.section.1).
collectionLinks :: Traversal' Document Text
collectionLinks =
     root
  .  named "service"
  ./ named "workspace"
  ./ named "collection"
  .  attr  "href"

-- | Traversal of an OData service document (an XML Document) focusing on all
-- collection titles. (Follow terminology found in
-- http://bitworking.org/projects/atom/rfc5023.html#rfc.section.1).
collectionTitles :: Traversal' Document Text
collectionTitles =
     root
  .  named "service"
  ./ named "workspace"
  ./ named "collection"
  ./ named "title"
  .  text

readCollectionList :: IO [Text]
readCollectionList = do
  service <- readDocument ""
  return $ service ^.. collectionLinks
