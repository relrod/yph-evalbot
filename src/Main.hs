{-# LANGUAGE OverloadedStrings #-}
module Main where

import Evaluate

import Control.Lens
import Control.Monad (void)
import Control.Monad.IO.Class
import Data.Aeson (Value)
import Data.Aeson.Lens
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import Data.Time.Clock.POSIX
import HTMLEntities.Decoder (htmlEncodedText)
import qualified Network.Wreq as W
import System.Environment (getEnv)
import Text.Printf
import Web.Slack
import Web.Slack.Message
import Web.Slack.State

genConfig :: IO SlackConfig
genConfig = SlackConfig <$> getEnv "EVALBOT_TOKEN"

-- This is copied from upstream but without annoying the MonadError constraint.
-- Instead we keep the MonadIO constraint, and simply return a disjunction.
makeSlackCall'
    :: MonadIO m
    => SlackConfig
    -> String
    -> (W.Options -> W.Options)
    -> m (Either T.Text Value)
makeSlackCall' conf method setArgs = do
    let url = "https://slack.com/api/" ++ method
    let setToken = W.param "token" .~ [T.pack (_slackApiToken conf)]
    let opts = W.defaults & setToken & setArgs
    rawResp <- liftIO $ W.getWith opts url
    let resp = rawResp ^? W.responseBody . _Value
    case resp of
      Just r ->
        case r ^? key "ok"  . _Bool of
          Just True  -> return . Right $ r
          Just False -> return . Left $ r ^. key "error" . _String
          Nothing    -> return . Left $ "Couldn't parse key 'ok' from response"
      Nothing -> return . Left $ "Unable to parse response"

postReply
  :: MonadIO m
  => SlackConfig
  -> ChannelId
  -> SlackTimeStamp
  -> T.Text
  -> m (Either T.Text Value)
postReply conf (Id ch) (SlackTimeStamp (Time pt) u) text =
  makeSlackCall' conf "chat.postMessage" args
  where
    args x = x &
             W.param "channel" .~ [ch] &
             W.param "thread_ts" .~ [ts] &
             W.param "text" .~ [text]
    ts = (T.pack . show . floor . toRational $ pt) <> (T.pack $ printf ".%06d" u)

postFileComment
  :: MonadIO m
  => SlackConfig
  -> FileId
  -> T.Text
  -> m (Either T.Text Value)
postFileComment conf (Id fid) text =
  makeSlackCall' conf "files.comments.add" args
  where
    args x = x &
             W.param "file" .~ [fid] &
             W.param "comment" .~ [text]

evalbot :: SlackBot ()
evalbot (Message c s t ts st e) = processMessage c s t ts st e
evalbot (FileShared ref Nothing ts) = processFileShared ref ts
evalbot x = liftIO (print x) >> return ()

processRequest :: T.Text -> IO (Maybe T.Text)
processRequest msg
  | "!eval" `T.isInfixOf` msg && "```" `T.isInfixOf` msg =
      let code = T.dropAround (== '`') . snd . T.breakOn (T.pack "```")
                 . fst . T.breakOnEnd (T.pack "```") $ msg
          lang = listToMaybe . drop 1 . dropWhile (/= T.pack "!eval")
                 . T.words $ msg
      in case routeEval (fromMaybe "" lang) of
        Nothing -> return . Just $ "Sorry I cannot evaluate language " <>
                   (fromMaybe "<unknown>" lang)
        Just fn -> do
          let decodedCode = TL.toStrict . TL.toLazyText . htmlEncodedText $ code
              filename = "/tmp/eval/" ++ T.unpack (sourceFilename fn)
          T.writeFile filename decodedCode
          res <- doEval fn
          return (Just res)
  | otherwise = return Nothing

processMessage
  :: ChannelId
  -> Submitter
  -> T.Text
  -> SlackTimeStamp
  -> Maybe Subtype
  -> Maybe Edited
  -> Slack s ()
processMessage c s t ts Nothing e
  | "!languages" `T.isInfixOf` t =
      sendMessage c $ "I support " <> T.intercalate ", " languages
  | otherwise = do
      conf <- use config
      evalResult <- liftIO $ processRequest t
      case evalResult of
        Just er -> do
          res <- postReply conf c ts ("```" <> er <> "```")
          liftIO $ print res
        Nothing -> liftIO $ print "Message didn't match"
      return ()
processMessage _ _ _ _ _ _ = return ()

processEvalFile :: Value -> SlackSession -> Maybe (IO T.Text)
processEvalFile v sess = do
  let inChannels = sess ^. slackChannels
  lang <- v ^? key "file" . key "filetype" . _String
  code <- v ^? key "content" . _String
  fn <- routeEval lang
  let decodedCode = TL.toStrict . TL.toLazyText . htmlEncodedText $ code
      filename = "/tmp/eval/" ++ T.unpack (sourceFilename fn)
  return $ do
    T.writeFile filename decodedCode
    doEval fn

processFileShared :: FileReference -> SlackTimeStamp -> Slack s ()
processFileShared (FileReference f@(Id fid)) ts = do
  conf <- use config
  sess <- use session
  info <- makeSlackCall' conf "files.info" (
    \args -> args & W.param "file" .~ [fid])
  case info of
    Left e -> liftIO $ print e
    Right v -> liftIO $ case processEvalFile v sess of
      Nothing -> return ()
      Just res -> do
        r <- res
        z <- postFileComment conf f ("```" <> r <> "```")
        print z

main :: IO ()
main = do
  conf <- genConfig
  runBot conf evalbot ()
