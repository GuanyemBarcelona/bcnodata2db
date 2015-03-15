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
