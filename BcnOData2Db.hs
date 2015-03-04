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
import           Data.Maybe
import           Data.Text           (Text)
import           Database.Persist.TH
import           Text.XML
import           Text.XML.Lens

-- |
-- = OData resources

share
  [mkPersist sqlSettings, mkMigrate "migrateAll"]
  [persistLowerCase|
    DivisioTerritorial sql=divisions_territorials
      -- We can use enumerated types in persistent, but they require a more
      -- involved use of TH that breaks Emacs flycheck. They are translated into
      -- SQL varchars anyway.
      -- categoria_divisio CategoriaDivisio
      codi_divisio_territorial          Text                sqltype=varchar(4)
      nom_divisio_territorial           Text
      categoria_divisio                 Text
      codi_divisio_territorial_pare     Text Maybe          sqltype=varchar(4)
      Primary codi_divisio_territorial
      deriving Show
  |]

divisioTerritorial :: ReifiedFold Element DivisioTerritorial
divisioTerritorial = DivisioTerritorial
  <$> Fold ( propText "codi_divisio_territorial"       . to fromJust )
  <*> Fold ( propText "nom_divisio_territorial"        . to fromJust )
  <*> Fold ( propText "categoria_divisio"              . to fromJust )
  <*> Fold ( propText "codi_divisio_territorial_pare"                )

-- |
-- = Main program and auxiliary functions

main :: IO ()
main = do
  document <- readDocument
  let entryList = document ^.. entries . runFold divisioTerritorial
  mapM_ print entryList
  print $ length entryList
  return ()

readDocument :: IO Document
readDocument = Text.XML.readFile def "examples/OPENDATADIVTER0.xml"

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
