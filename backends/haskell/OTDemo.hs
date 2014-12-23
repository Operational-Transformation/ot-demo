{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Yesod.Core as YC
import qualified Yesod.Static as YS
--import qualified Network.EngineIO as EIO
import qualified Network.SocketIO as SIO
import qualified Data.Text as T
import qualified Control.OperationalTransformation.Server as OTS
import qualified Control.OperationalTransformation.Text as OTT
import qualified Control.OperationalTransformation.Selection as Sel
import Network.EngineIO.Yesod (yesodAPI)
import Control.Monad.State.Strict (StateT)
import Control.Monad.Trans.Reader (ReaderT (..))
import Control.Monad.Reader (MonadReader (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad (mzero)
import Control.Applicative
import qualified Control.Concurrent.STM as STM
import Data.Aeson
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as BSC8
import Control.Monad (unless)
import Data.Monoid

{-
  TODO: import file paths from cabal
-}

data ClientState = ClientState
  { clientName :: !T.Text
  , clientSelection :: !Sel.Selection
  } deriving (Show)

instance ToJSON ClientState where
  toJSON (ClientState name sel) =
    object $ [ "name" .= name ] ++ (if sel == mempty then [] else [ "selection" .= sel ])

data OTDemo = OTDemo
  { getStatic :: YS.Static
  , socketIOHandler :: YC.HandlerT OTDemo IO ()
  , serverState :: STM.TVar (OTS.ServerState T.Text OTT.TextOperation)
  , clients :: STM.TVar (M.Map T.Text ClientState)
  }

YC.mkYesod "OTDemo" [YC.parseRoutes|
/ IndexR GET
/static StaticR YS.Static getStatic
/socket.io/ SocketIOR
|]

instance YC.Yesod OTDemo where
  -- do not redirect /socket.io/?bla=blub to /socket.io?bla=blub
  cleanPath _ ["socket.io",""] = Right ["socket.io"]
  cleanPath _ s =
    if corrected == s
        then Right $ map dropDash s
        else Left corrected
    where
      corrected = filter (not . T.null) s
      dropDash t
          | T.all (== '-') t = T.drop 1 t
          | otherwise = t

getIndexR :: Handler ()
getIndexR = YC.sendFile "text/html" "../../public/index.html"

handleSocketIOR :: Handler ()
handleSocketIOR = YC.getYesod >>= socketIOHandler

main :: IO ()
main = do
  getStatic <- YS.static "../../public"
  socketIOHandler <- SIO.initialize yesodAPI server
  serverState <- STM.newTVarIO (OTS.initialServerState "baba links")
  clients <- STM.newTVarIO M.empty
  YC.warp 8000 (OTDemo {..})

-----------------------

newtype Revision = Revision { getRevisionNumber :: Integer } deriving (Num, FromJSON, ToJSON)

data UserLogin = UserLogin T.Text

instance FromJSON UserLogin where
  parseJSON (Object o) = UserLogin <$> o .: "name"
  parseJSON _ = mzero

server :: StateT SIO.RoutingTable (ReaderT SIO.Socket Handler) ()
server = do
  OTDemo {..} <- YC.getYesod
  s <- ask
  let sid = T.pack . BSC8.unpack $ SIO.socketId s

  mayEditTV <- liftIO $ STM.newTVarIO False
  let mayEdit = liftIO $ STM.readTVarIO mayEditTV

  SIO.on "login" $ \(UserLogin name) -> do
    let client = ClientState name mempty
    liftIO $ STM.atomically $ do
      STM.writeTVar mayEditTV True
      STM.modifyTVar clients (M.insert sid client)
    SIO.broadcastJSON "set_name" [toJSON sid, toJSON name]
    SIO.emitJSON "logged_in" []

  SIO.on "operation" $ \rev op (sel :: Sel.Selection) -> do
    me <- mayEdit
    unless me $ fail "user is not allowed to make any changes"
    res <- liftIO $ STM.atomically $ do
     ss <- STM.readTVar serverState
     case OTS.applyOperation ss rev op sel of
       Left err -> return (Left err)
       Right (op', sel', ss') -> do
         STM.writeTVar serverState ss'
         return $ Right (op', sel')
    case res of
      Left err -> liftIO $ putStrLn err
      Right (op', sel') -> do
        liftIO $ putStrLn $ "new operation: " ++ show op'
        SIO.emitJSON "ack" []
        SIO.broadcastJSON "operation" [toJSON sid, toJSON op', toJSON sel']

  SIO.on "selection" $ \sel -> do
    me <- mayEdit
    unless me $ fail "user is not allowed to select anything"
    liftIO $ STM.atomically $
      STM.modifyTVar clients (M.adjust (\u -> u { clientSelection = sel }) sid)
    SIO.broadcastJSON "selection" [toJSON sid, toJSON sel]

  SIO.appendDisconnectHandler $ const $ do
    STM.atomically $ STM.modifyTVar clients (M.delete sid)
    runReaderT (SIO.broadcast "client_left" sid) s

  -- send initial message
  currClients <- liftIO $ STM.readTVarIO clients
  OTS.ServerState rev doc _ <- liftIO $ STM.readTVarIO serverState
  SIO.emit "doc" $ object
    [ "str" .= doc
    , "revision" .= rev
    , "clients" .= currClients
    ]
