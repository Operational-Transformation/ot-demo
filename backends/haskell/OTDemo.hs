{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}

module Main where

import Yesod
import Yesod.Static
import Network.Wai.Handler.Warp (run)
import Data.Text (Text, pack)
import Control.Concurrent.MVar
import Control.OperationalTransformation.Server
import Control.OperationalTransformation.Text
import Data.Monoid (mconcat)
import Control.Concurrent.Broadcast
import Control.Applicative ((<$>))

data OTDemo = OTDemo
  { otState   :: MVar (ServerState Text TextOperation, Broadcast TextOperation)
  , getStatic :: Static
  }

mkYesod "OTDemo" [parseRoutes|
  / RootR GET
  /static StaticR Static getStatic
  /login/#Text LoginR GET
  /ot OTR GET
  /ot/revision/#Revision OTRevisionR GET POST
  /ot/cursor OTCursorR POST
|]

instance Yesod OTDemo where

main :: IO ()
main = do
  b <- new
  ot <- newMVar (initialServerState initialText, b)
  s <- staticDevel "../../public"
  warpDebug 3000 $ OTDemo ot s
  where
    initialText = mconcat
      [ "# This is a Markdown heading\n\n"
      , "1. un\n"
      , "2. deux\n"
      , "3. trois\n\n"
      , "Lorem *ipsum* dolor **sit** amet.\n\n"
      , "    $ touch test.txt"
      ]

jsonError :: Text -> Handler RepJson
jsonError msg = jsonToRepJson $ object [ "error" .= msg ]

getRootR :: Handler RepHtml
getRootR = sendFile typeHtml "../../public/index.html"

getLoginR :: Text -> Handler RepJson
getLoginR name = do
  setSession "name" name
  jsonToRepJson (object [ "ok" .= ("logged in" :: Text) ])

getOTR :: Handler RepJson
getOTR = do
  (ServerState rev doc _, _) <- getYesod >>= liftIO . readMVar . otState
  jsonToRepJson $ object [ "revision" .= rev, "document" .= doc ]

getOTRevisionR :: Revision -> Handler RepJson
getOTRevisionR r = do
  (ServerState rev _ ops, newOp) <- getYesod >>= liftIO . readMVar . otState
  if r < rev
    then jsonToRepJson $ object [ "operations" .= reverse (take (fromIntegral (rev-r)) ops) ]
    else do
      op <- liftIO $ listen newOp
      jsonToRepJson $ object [ "operations" .= [op] ]

postOTRevisionR :: Revision -> Handler RepJson
postOTRevisionR rev = do
  AugmentedTextOperation _ operation <- parseJsonBody_
  otMVar <- otState <$> getYesod
  (serverState, broadcastChannel) <-  liftIO $ takeMVar otMVar
  case applyOperation serverState rev operation of
    Left err -> do
      liftIO $ putMVar otMVar (serverState, broadcastChannel)
      jsonError (pack err)
    Right (operation', serverState') -> do
      liftIO $ do
        signal broadcastChannel operation'
        putMVar otMVar (serverState', broadcastChannel)
      getOTRevisionR rev

postOTCursorR :: Handler RepJson
postOTCursorR = jsonToRepJson (object ([] :: [(Text, Int)]))