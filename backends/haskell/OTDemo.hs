{-# LANGUAGE TemplateHaskell, QuasiQuotes, TypeFamilies, MultiParamTypeClasses, OverloadedStrings #-}

module Main where

import Yesod
import Yesod.Static
import Data.Text (Text, pack, split)
import Control.Concurrent.MVar
import Control.OperationalTransformation
import Control.OperationalTransformation.Server
import Control.OperationalTransformation.Text
import Data.Monoid (mconcat, (<>))
import Control.Concurrent
import Control.Concurrent.Broadcast
import Control.Applicative ((<$>), (<*>))
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import Control.Monad (foldM, forever)
import Network.HTTP.Types.Status
import Data.Aeson as A
import Data.Aeson.Types
--import Data.Function (fix)

data ComplexRevision = ComplexRevision
  { majorRev :: Revision
  , minorRev :: Revision
  } deriving (Show, Read, Eq)

instance Ord ComplexRevision where
  compare (ComplexRevision aMaj aMin) (ComplexRevision bMaj bMin) =
    case compare aMaj bMaj of
      EQ -> compare aMin bMin
      x  -> x

instance ToJSON ComplexRevision where
  toJSON (ComplexRevision major minor) = A.object [ "major" .= major, "minor" .= minor ]

instance PathPiece ComplexRevision where
  toPathPiece (ComplexRevision major minor) = toPathPiece major <> "-" <> toPathPiece minor
  fromPathPiece s = case Data.Text.split (== '-') s of
    [major] -> ComplexRevision <$> fromPathPiece major <*> return 0
    [major, minor] -> ComplexRevision <$> fromPathPiece major <*> fromPathPiece minor
    _ -> Nothing

incMajor, incMinor :: ComplexRevision -> ComplexRevision
incMajor (ComplexRevision major _)     = ComplexRevision (major+1) 0
incMinor (ComplexRevision major minor) = ComplexRevision major (minor+1)

data OTDoc = OTDoc
  { otDocContent   :: Text
  , otDocHistory   :: [UserEvent TextOperation]
  , otDocRevision  :: ComplexRevision
  , otDocUsers     :: M.Map Text (Bool, Maybe Cursor)
  , otDocEvents    :: [UserEvent UserAction]
  , otDocBroadcast :: Broadcast (Maybe (UserEvent TextOperation), [UserEvent UserAction])
  }

getOperationsSince :: OTDoc -> Revision -> [UserEvent TextOperation]
getOperationsSince otDoc major = reverse $ take newOps $ otDocHistory otDoc
  where newOps = fromIntegral (majorRev (otDocRevision otDoc) - major)

data OTDemo = OTDemo
  { otState   :: MVar OTDoc
  , getStatic :: Static
  }

data OperationWithCursor = OperationWithCursor TextOperation (Maybe Cursor)

instance FromJSON OperationWithCursor where
  parseJSON (Object o) = OperationWithCursor <$> o .: "operation" <*> o .:? "cursor"
  parseJSON _ = fail "expected an object"

type UserName = Text

data UserEvent a = UserEvent
  { userEventUser :: UserName
  , userEventData :: a
  }

{-
instance (OTOperation op) => OTOperation (UserEvent op) where
  transform (UserEvent u1 op1) (UserEvent u2 op2) = do
    (op1', op2') <- transform op1 op2
    return (UserEvent u1 op1', UserEvent u2 op2')
-}

class UserEventData a where
  toEventJSON :: a -> [Pair]

instance UserEventData TextOperation where
  toEventJSON = (:[]) . ("operation" .=)

instance UserEventData a => ToJSON (UserEvent a) where
  toJSON (UserEvent user payload) = A.object $ ("user" .= user) : toEventJSON payload

data UserAction = UserJoined | UserLeft | UserCursorUpdate (Maybe Cursor)

instance UserEventData UserAction where
  toEventJSON UserJoined = [ "event" .= ("joined" :: Text) ]
  toEventJSON UserLeft   = [ "event" .= ("left"   :: Text) ]
  toEventJSON (UserCursorUpdate c) =
    [ "event"  .= ("cursor"   :: Text)
    , "cursor" .= maybe Null toJSON c
    ]

--data AnnotatedOperation = AnnotatedOperation UserName TextOperation

mkYesod "OTDemo" [parseRoutes|
  / RootR GET
  /static StaticR Static getStatic
  /login/#Text LoginR GET
  /ot OTR GET
  /ot/revision/#ComplexRevision OTRevisionR GET POST
  /ot/revision/#ComplexRevision/cursor OTCursorR POST
|]

instance Yesod OTDemo where
  isAuthorized _ False = return Authorized -- a read request
  isAuthorized _ True  = maybe (Unauthorized "login required") (const Authorized) <$> maybeUserName

main :: IO ()
main = do
  bc <- new
  otDoc <- newMVar $ mkOtDoc bc
  s <- staticDevel "../../public"
  _ <- liftIO $ forkIO $ timeoutThread otDoc
  warpDebug 3000 $ OTDemo otDoc s
  where
    mkOtDoc bc = OTDoc
      { otDocContent   = initialText
      , otDocHistory   = []
      , otDocRevision  = ComplexRevision 0 0
      , otDocUsers     = M.empty
      , otDocEvents    = []
      , otDocBroadcast = bc
      }
    initialText = mconcat
      [ "# This is a Markdown heading\n\n"
      , "1. un\n"
      , "2. deux\n"
      , "3. trois\n\n"
      , "Lorem *ipsum* dolor **sit** amet.\n\n"
      , "    $ touch test.txt"
      ]

timeoutThread :: MVar OTDoc -> IO ()
timeoutThread mv = forever $ do
  threadDelay $ 10 * 1000000 -- 10 seconds
  modifyMVar_ mv $ \otDoc -> do
    let (active, inactive) = M.partition fst $ otDocUsers otDoc
    let events = (\u -> UserEvent u UserLeft) <$> M.keys inactive
        rev = otDocRevision otDoc
    signal (otDocBroadcast otDoc) (Nothing, events)
    return $ otDoc
      { otDocUsers = (,) False . snd <$> active
      , otDocRevision = ComplexRevision (majorRev rev) (minorRev rev + fromIntegral (length events))
      , otDocEvents = events ++ otDocEvents otDoc
      }

sendJsonError :: Status -> Text -> Handler RepJson
sendJsonError status msg = do
  repJson <- jsonToRepJson $ A.object [ "error" .= msg ]
  sendResponseStatus status repJson

sendEvent :: OTDoc -> UserEvent UserAction -> IO OTDoc
sendEvent otDoc event = do
  signal (otDocBroadcast otDoc) (Nothing, [event])
  return otDoc'
  where
    otDoc' = otDoc
      { otDocRevision = incMinor (otDocRevision otDoc)
      , otDocEvents   = event : otDocEvents otDoc
      }

getRootR :: Handler RepHtml
getRootR = sendFile typeHtml "../../public/index.html"

getLoginR :: Text -> Handler RepJson
getLoginR name = do
  setSession "name" name
  let event = UserEvent name UserJoined
  mv <- otState <$> getYesod
  liftIO $ modifyMVar_ mv $ \otDoc -> do
    let otDoc' = otDoc { otDocUsers = M.insert name (True, Nothing) (otDocUsers otDoc) }
    sendEvent otDoc' event
  jsonToRepJson $ A.object [ "ok" .= ("logged in" :: Text) ]

maybeUserName :: GHandler s OTDemo (Maybe UserName)
maybeUserName = lookupSession "name"

requireUserName :: GHandler s OTDemo UserName
requireUserName = maybeUserName >>= maybe (fail "Login required") return

getOTR :: Handler RepJson
getOTR = do
  otDoc <- getYesod >>= liftIO . readMVar . otState
  let OTDoc { otDocContent = str, otDocRevision = rev, otDocUsers = users } = otDoc
  jsonToRepJson $ A.object
    [ "revision" .= rev
    , "document" .= str
    , "users"    .= (maybe Null toJSON . snd <$> users)
    ]

getOTRevisionR :: ComplexRevision -> Handler RepJson
getOTRevisionR r = do
  mv <- otState <$> getYesod
  otDoc <- liftIO $ takeMVar mv
  mUserName <- maybeUserName
  otDoc' <- case mUserName of
    Nothing -> return otDoc
    Just userName | userName `M.member` otDocUsers otDoc -> return $ otDoc
      { otDocUsers = M.adjust ((,) True . snd) userName (otDocUsers otDoc)
      }
    Just userName -> do
      let event = UserEvent userName UserJoined
          otDoc' = otDoc { otDocUsers = M.insert userName (True, Nothing) (otDocUsers otDoc) }
      liftIO $ sendEvent otDoc' event
  liftIO $ putMVar mv otDoc'
  let OTDoc { otDocRevision = rev, otDocBroadcast = bc
            , otDocUsers = users, otDocEvents = events } = otDoc'
  if majorRev r < majorRev rev
    then do
      jsonToRepJson $ A.object
        [ "operations" .= getOperationsSince otDoc' (majorRev r)
        , "users"      .= (maybe Null toJSON . snd <$> users)
        , "revision"   .= rev
        ]
    else if minorRev r < minorRev rev
      then do
        let newEvents = reverse (take (fromIntegral (minorRev rev - minorRev r)) events)
        jsonToRepJson $ A.object [ "operations" .= emptyArray, "events" .= newEvents ]
      else do
        (newOp, newEvents) <- liftIO $ listen bc
        jsonToRepJson $ A.object
              [ "operations" .= maybeToList newOp
              , "events" .= newEvents
              ]
        {-
        userName <- requireUserName
        fix $ \loop -> do
          news <- liftIO $ listen bc
          case news of
            (Nothing, [UserEvent user _]) | user == userName -> loop
            (newOp, newEvents) -> jsonToRepJson $ A.object
              [ "operations" .= maybeToList newOp
              , "events" .= newEvents
              ]
        -}

transformIncomingOperation :: ComplexRevision
                           -> UserEvent TextOperation
                           -> Maybe Cursor
                           -> OTDoc
                           -> Either String (UserEvent TextOperation, Maybe Cursor, OTDoc)
transformIncomingOperation oprev (UserEvent user op) cursor otDoc = do
  concurrentOps <- if oprev > rev || majorRev rev - majorRev oprev > fromIntegral (length ops)
    then Left "unknown revision number"
    else Right $ take (fromInteger $ majorRev rev - majorRev oprev) ops
  (op', cursor') <- foldM transformFst (op, cursor) (userEventData <$> reverse concurrentOps)
  contents' <- case apply op' contents of
    Left err -> Left $ "apply failed: " ++ err
    Right d -> Right d
  let otDoc' = otDoc
        { otDocContent  = contents'
        , otDocRevision = incMajor rev
        , otDocHistory  = (UserEvent user op'):ops
        , otDocUsers    = (,) True . fmap (flip updateCursor op') . snd <$> otDocUsers otDoc
        }
  return (UserEvent user op', cursor', otDoc')
  where
    OTDoc { otDocContent = contents, otDocRevision = rev, otDocHistory = ops } = otDoc
    transformFst (a, cursor_) b = case transform a b of
      Left err -> Left $ "transform failed: " ++ err
      Right (a', b') -> Right (a', flip updateCursor b' <$> cursor_)

postOTRevisionR :: ComplexRevision -> Handler RepJson
postOTRevisionR rev = do
  OperationWithCursor operation cursor <- parseJsonBody_
  otMVar <- otState <$> getYesod
  otDoc <- liftIO $ takeMVar otMVar
  user <- requireUserName
  case transformIncomingOperation rev (UserEvent user operation) cursor otDoc of
    Left err -> do
      liftIO $ putMVar otMVar otDoc
      sendJsonError badRequest400 (pack err)
    Right (operation', cursor', otDoc') -> do
      let cursorUpdate = UserEvent user (UserCursorUpdate cursor')
          otDoc'' = otDoc'
            { otDocEvents = [cursorUpdate]
            , otDocRevision = incMinor (otDocRevision otDoc')
            }
      liftIO $ do
        signal (otDocBroadcast otDoc) (Just operation', [cursorUpdate])
        putMVar otMVar otDoc''
      jsonToRepJson $ A.object [ "ok" .= ("document updated" :: Text) ]

postOTCursorR :: ComplexRevision -> Handler RepJson
postOTCursorR (ComplexRevision major _) = do
  cursorR <- parseJsonBody
  let cursor = case cursorR of
        Error _   -> Nothing
        Success c -> Just c
  mv <- otState <$> getYesod
  user <- requireUserName
  liftIO $ modifyMVar_ mv $ \otDoc -> do
    let newOperations = getOperationsSince otDoc major
        cursor' = flip (foldl updateCursor) (userEventData <$> newOperations) <$> cursor
        otDoc'  = otDoc { otDocUsers = M.insert user (True, cursor') (otDocUsers otDoc) }
    sendEvent otDoc' $ UserEvent user $ UserCursorUpdate cursor'
  jsonToRepJson $ A.object [ "ok" .= ("cursor updated" :: Text) ]