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
      taulaIFont         Text        -- Where the data is stored ++ its source,
                                     -- usually a URL
      tipus              Text        -- E.g. "OData-XML"
      darreraDescarrega  UTCTime     -- SQL type: DATE
      origen             Text        -- Institution that generated the data set
      comentari          Text        -- E.g. limitations, transformations done.
      url1               Text        -- Pointer to metainfo with, e.g, a
                                     -- description of every field
      url2               Text
      UniqueTaulaIFont taulaIFont    -- Cannot simply create a composite key
                                     -- with this as repsert fails and need to
                                     -- use upsert

    Districtes
      codiDistricte        Int
      nomDistricte         Text
      Primary codiDistricte
      UniqueNomDistricte nomDistricte
      deriving Show

    Barris
      codiBarri        Int
      nomBarri         Text
      codiDistricte    Int Maybe
      urlFitxaBarri    Text Maybe
      Primary codiBarri
      UniqueNomBarri nomBarri
      Foreign Districtes fkey codiDistricte
      deriving Show

    MigrantsPerSexe
      codiBarri        Int
      any_             Int
      tipus            Text             sqltype=varchar(1)     -- "I" | "E"
      codiDistricte    Int Maybe
      dones            Int
      homes            Int
      total            Int
      Primary codiBarri any_ tipus
      Foreign Barris fkeyBarri codiBarri
      Foreign Districtes fkeyDistricte codiDistricte
      deriving Show

    AturatsEvolucioMensual
      codiBarri        Int
      any_             Int
      mes              Int
      codiDistricte    Int Maybe
      num              Int
      Primary codiBarri any_ mes
      Foreign Barris fkeyBarri codiBarri
      Foreign Districtes fkeyDistricte codiDistricte
      deriving Show
  |]
