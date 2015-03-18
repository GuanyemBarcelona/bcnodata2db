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
import           Data.Time.Clock
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
      taula_i_font       Text        -- Where the data is stored ++ its source,
                                     -- usually a URL
      tipus              Text        -- E.g. "OData-XML"
      darrera_descarrega UTCTime     -- SQL type: DATE
      origen             Text Maybe  -- Institution that generated the data set
      transformacio      Text Maybe
      url1               Text Maybe  -- Pointer to metainfo with, e.g, a
                                     -- description of every field
      url2               Text Maybe
      UniqueTaulaIFont taula_i_font  -- Cannot simply create a composite key
                                     -- with this as repsert fails and need to
                                     -- use upsert

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

    MigrantsPerSexe
      codi_barri       Int
      any_             Int
      tipus            Text             sqltype=varchar(1)     -- "I" | "E"
      codi_districte   Int Maybe
      dones            Int
      homes            Int
      total            Int
      Primary codi_barri any_ tipus
      Foreign Barris fkeyBarri codi_barri
      Foreign Districtes fkeyDistricte codi_districte
      deriving Show

    AturatsEvolucioMensual
      codi_barri       Int
      any_             Int
      mes              Int
      codi_districte   Int Maybe
      num              Int
      Primary codi_barri any_ mes
      Foreign Barris fkeyBarri codi_barri
      Foreign Districtes fkeyDistricte codi_districte
      deriving Show
  |]
