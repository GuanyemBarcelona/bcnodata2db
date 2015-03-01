{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Data.Text (Text)
-- import qualified Data.Text as T
import           Database.Persist.TH
import           Text.XML
import           Text.XML.Lens

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    DivisioTerritorial sql=divisions_territorials
      codi_divisio_territorial Text sqltype=varchar(4)
      nom_divisio_territorial Text
      -- categoria_divisio CategoriaDivisio
      categoria_divisio Text
      codi_divisio_territorial_pare Text Maybe sqltype=varchar(4)
      Primary codi_divisio_territorial
      deriving Show
  |]

main :: IO ()
main = do
  document <- Text.XML.readFile def "examples/OPENDATADIVTER0.xml"
  let l = toListOf (objects . to divisioTerritorial) document
  mapM_ print (take 5 l)
  return ()

-- | Traversal of an OData (XML) Document focusing on all rows.
objects :: Applicative f => (Element -> f Element) -> Document -> f Document
objects =    root
          .  named "feed"
          ./ named "entry"
          ./ named "content"
          ./ named "properties"

divisioTerritorial :: Element -> DivisioTerritorial
divisioTerritorial object =
  DivisioTerritorial
    (view    (plate . named "codi_divisio_territorial"      . text) object)
    (view    (plate . named "nom_divisio_territorial"       . text) object)
    (view    (plate . named "categoria_divisio"             . text) object)
    (preview (plate . named "codi_divisio_territorial_pare" . text) object)
