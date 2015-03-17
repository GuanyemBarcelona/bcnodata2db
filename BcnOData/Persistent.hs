{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module BcnOData.Persistent where

import           Data.Text                   (Text)
import           Database.Persist.TH


-- |
-- = OData resource collections

share
  [ mkPersist sqlSettings { mpsPrefixFields   = True
                          , mpsGeneric        = False
                          , mpsGenerateLenses = True
                          }
  , mkMigrate "migrateAll"
  ]
  [persistLowerCase|
    FontsDeDades
      font          Text        -- Usually a URL, or simply an id string
      tipus         Text        -- E.g. "OData-XML"
      taula         Text        -- Where the data is stored
      origen        Text        -- Institution that generated the data set
      transformacio Text Maybe
      url1          Text Maybe  -- Pointer to metainfo with, e.g, a description
                                -- of every field
      url2          Text Maybe
      url3          Text Maybe
      Primary font

    Districtes
      codi_districte        Int
      nom_districte         Text
      Primary codi_districte
      UniqueNomDistricte nom_districte
      deriving Show

    Barris
      codi_barri       Int
      nom_barri        Text
      codi_districte   Int Maybe
      url_fitxa_barri  Text Maybe
      Primary codi_barri
      UniqueNomBarri nom_barri
      Foreign Districtes fkey codi_districte
      deriving Show

    -- http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/emigracio-sexe/
    -- http://opendata.bcn.cat/opendata/ca/catalog/DEMOGRAFIA/immigracio-sexe/
    -- http://www.bcn.cat/estadistica/catala/dades/barris/tdemo/index.htm
    MigrantsPerSexe
      codi_barri       Int
      any_             Int
      tipus            Text             sqltype=varchar(1)     -- "I" | "E"
      codi_districte   Int Maybe
      dones            Int
      homes            Int
      total            Int
      Primary codi_barri any_ tipus
      Foreign Barris fkeyBarri codi_districte
      Foreign Districtes fkeyDistricte codi_districte
      deriving Show

    OpenDataImmigracioSexe2013
      codi_barri      Int       -- barris 74 ["44. Vilapicina i la Torre
                                --            Llobeta","52. la Prosperitat"]
      codi_districte  Int Maybe -- dte 73 ["8","8"]
      dones           Int       -- 75 ["421","469"]
      homes           Int       -- 75 ["438","458"]
      total           Int       -- 75 ["859","927"]
      -- Foreign DivisioTerritorial fkey codi_barri
      Primary codi_barri
      deriving Show
  |]
