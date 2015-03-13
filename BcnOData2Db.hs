{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class      (liftIO)
import           Control.Monad.Logger        (runStderrLoggingT)
import qualified Data.ByteString.Char8       as B8 (pack)
import           Data.Char
import           Data.List
import qualified Data.Map.Strict             as M
import           Data.Maybe
import qualified Data.Set                    as S
import           Data.Text                   (Text)
import qualified Data.Text                   as T
import           Database.Persist
import           Database.Persist.Postgresql
import           Database.Persist.TH
import           Network                     (withSocketsDo)
import           Network.HTTP.Conduit
import           Options.Applicative
import           Text.XML
import           Text.XML.Lens


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

    OpenDataImmigracioSexe2013
      codi_barri     Int Maybe -- barris 74 ["44. Vilapicina i la Torre
                               -- Llobeta","52. la Prosperitat"]
      codi_districte Int Maybe -- dte 73 ["8","8"]
      dones          Int       -- 75 ["421","469"]
      homes          Int       -- 75 ["438","458"]
      total          Int       -- 75 ["859","927"]
      comentari      Text Maybe
      -- Foreign DivisioTerritorial fkey codi_barri
      UniqueCodiBarri codi_barri !force
      deriving Show
  |]

divisioTerritorial :: ReifiedFold Element DivisioTerritorial
divisioTerritorial = DivisioTerritorial
  <$> Fold ( propText "codi_divisio_territorial"       . to fromJust )
  <*> Fold ( propText "nom_divisio_territorial"        . to fromJust )
  <*> Fold ( propText "categoria_divisio"              . to fromJust )
  <*> Fold ( propText "codi_divisio_territorial_pare"                )
  <*> Fold ( propText "url_fitxa_divisio_territorial"                )

openDataImmigracioSexe2013 :: ReifiedFold Element OpenDataImmigracioSexe2013
openDataImmigracioSexe2013 = OpenDataImmigracioSexe2013
  <$> Fold ( propText "barris" . to handleComment . toTakeNumPrefix . toRead )
  <*> Fold ( propText "dte"    . toRead                                      )
  <*> Fold ( propText "dones"  . toRead . to fromJust                        )
  <*> Fold ( propText "homes"  . toRead . to fromJust                        )
  <*> Fold ( propText "total"  . toRead . to fromJust                        )
  <*> Fold ( propText "barris" . to handleComment'                           )
  where
    handleComment  (Just "No consta") = Nothing
    handleComment  b                  = b
    handleComment' (Just "No consta") = Just "Barri no consta"
    handleComment' _                  = Nothing


-- |
-- = Helper functions for defining OData collection folds

toRead :: (Contravariant f, Profunctor p, Read b, Functor f1) =>
          p (f1 b) (f (f1 b)) -> p (f1 Text) (f (f1 Text))
toRead = to (fmap T.unpack) . to (fmap read)

toTakeNumPrefix :: (Contravariant f, Profunctor p, Functor f1) =>
                   Optical' p p f (f1 Text) (f1 Text)
toTakeNumPrefix = to $ fmap (T.takeWhile isNumber)


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
main = execParser options' >>= \(Options d u p) -> handle non2xxStatusExp $ do
  document <- readDocument "OPENDATADIVTER0"
  let resources = document ^.. memberResources . runFold divisioTerritorial
  document' <- readDocument "OPENDATAIMMIGRACIOSEXE2013"
  let resources' = document' ^.. memberResources . runFold openDataImmigracioSexe2013
  runStderrLoggingT $ withPostgresqlPool (pqConnOpts d u p) 10 $ \pool ->
    liftIO $ flip runSqlPersistMPool pool $ do
      runMigration migrateAll
      insertMany $ sortBy districtesPrimer resources
      insertMany resources'
  return ()
    where
      districtesPrimer (DivisioTerritorial _ _ _ Nothing _) _ = LT
      districtesPrimer _ (DivisioTerritorial _ _ _ Nothing _) = GT
      districtesPrimer _ _                                    = EQ
      options' = info (helper <*> options) helpMessage
      pqConnOpts d u p = B8.pack $ concat [d, u, p]
      non2xxStatusExp :: HttpException -> IO ()
      non2xxStatusExp e = putStr "Caught " >> print e

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
  . filtered hasNonTrivialProperty
  where
    hasNonTrivialProperty :: Element -> Bool
    hasNonTrivialProperty e = any (`S.notMember` excluded) (subnodes e)
    subnodes :: Element -> [Text]
    subnodes e = e ^.. entire . localName

excluded :: S.Set Text
excluded = S.fromList [ "properties", "PartitionKey", "RowKey"
                      , "Timestamp", "entityid" ]

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
readDocument docName = withSocketsDo $ do
  result <- simpleHttp (odataBaseUrl ++ docName)
  return $ parseLBS_ def result

odataBaseUrl :: String
odataBaseUrl = "http://bcnodata.cloudapp.net:8080/v1/Data/"

-- collectionList :: [forall a. ReifiedFold Element a]
-- collectionList = [ _OPENDATAIMMIGRACIOSEXE2013, divisioTerritorial]


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
