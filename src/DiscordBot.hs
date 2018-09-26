{-# LANGUAGE OverloadedStrings, RankNTypes #-}

module Main where

import Control.Exception (finally)
import Control.Monad (forever, when)
import Data.Char (isSpace, toLower)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Lazy as TL
import System.Environment (getEnv)

import Discord
import Evaluate

type Dis z = (RestChan, Gateway, z)

mainLoop :: Dis z -> IO ()
mainLoop dis = forever $ do
  e <- nextEvent dis
  case e of
    MessageCreate m -> handleMessage dis m
    _ -> return ()

main :: IO ()
main = do
  tok <- T.pack <$> getEnv "EVALBOT_DISCORD_TOKEN"
  dis <- loginRestGateway (Auth tok)
  finally (mainLoop dis) (stopDiscord dis)

reply :: Dis z -> Message -> T.Text -> IO ()
reply dis m text = do
  resp <- restCall dis (CreateMessage (messageChannel m) text Nothing)
  putStrLn $ "[SENDING] " ++ show resp

handleMessage :: Dis z -> Message -> IO ()
handleMessage dis m = do
  print m
  when (T.pack "!" `T.isPrefixOf` messageText m) (handleCommand dis m)

handleCommand :: Dis z -> Message -> IO ()
handleCommand dis m
  | "!languages" `T.isPrefixOf` messageText m =
      reply dis m $ "I support " <> T.intercalate ", " languages
  | "!eval" `T.isPrefixOf` messageText m = handleEval dis m
  | otherwise = return ()

handleEval :: Dis z -> Message -> IO ()
handleEval dis m = do
  let msg = messageText m
      msgLines = T.lines msg
  when (length msgLines > 2 &&
         (T.pack "!eval" `T.isPrefixOf` head msgLines) &&
         (T.pack "```" `T.isPrefixOf` (msgLines !! 1))) $ do
    let codeFence = tail msgLines
        language = T.drop 3 (head codeFence)
        code = T.unlines . init . tail $ codeFence
    print language
    print code
    case routeEval language of
      Nothing -> reply dis m "That language isn't recognized. See !languages for languages I can evaluate."
      Just fn -> do
        let filename = "/tmp/eval/" ++ T.unpack (sourceFilename fn)
        TIO.writeFile filename code
        res <- doEval fn
        reply dis m ("```" <> T.replace "`" "\\`" res <> "```")
