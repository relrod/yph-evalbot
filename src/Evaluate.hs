{-# LANGUAGE OverloadedStrings #-}
module Evaluate where

import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Shelly as S

data Language =
  Language { compileCommand :: Maybe T.Text
           , evalCommand :: Maybe T.Text
           , sourceFilename :: T.Text
           } deriving (Eq, Show)


cPlusPlus :: T.Text -> Language
cPlusPlus fn =
  Language (Just ("g++ -Wall -pedantic eval/" <> fn)) (Just "./a.out") fn

c :: T.Text -> Language
c fn =
  Language (Just ("gcc -Wall -pedantic eval/" <> fn)) (Just "./a.out") fn

haskell :: T.Text -> Language
haskell fn =
  Language Nothing (Just ("runhaskell ./eval/" <> fn)) fn

bash :: T.Text -> Language
bash fn =
  Language Nothing (Just ("bash ./eval/" <> fn)) fn

ruby :: T.Text -> Language
ruby fn =
  Language Nothing (Just ("ruby ./eval/" <> fn)) fn

perl :: T.Text -> Language
perl fn =
  Language Nothing (Just ("perl ./eval/" <> fn)) fn

python2 :: T.Text -> Language
python2 fn =
  Language Nothing (Just ("python ./eval/" <> fn)) fn

coq :: T.Text -> Language
coq fn =
  Language
  Nothing
  (Just ("cp ./eval/" <> fn <> " . && coqc -color no " <> fn))
  fn

routeEval :: T.Text -> Maybe Language
routeEval "c++" = Just (cPlusPlus "eval.cxx")
routeEval "cpp" = Just (cPlusPlus "eval.cxx")
routeEval "c" = Just (c "eval.c")
routeEval "haskell" = Just (haskell "eval.hs")
routeEval "bash" = Just (bash "eval.sh")
routeEval "ruby" = Just (ruby "eval.rb")
routeEval "perl" = Just (perl "eval.pl")
routeEval "python2" = Just (python2 "eval.py")
routeEval "python" = Just (python2 "eval.py")
routeEval "coq" = Just (coq "eval.v")
routeEval _ = Nothing

languages :: [T.Text]
languages =
  sort [ "cpp", "c", "haskell", "bash", "ruby", "perl", "python2", "coq" ]

genCommand :: Language -> T.Text
genCommand (Language Nothing Nothing _) = ""
genCommand (Language (Just cc) Nothing _) = cc
genCommand (Language Nothing (Just ec) _) = ec
genCommand (Language (Just cc) (Just ec) _) = cc <> " && " <> ec

doEval :: Language -> IO T.Text
doEval lc = S.shelly . S.errExit False $ do
  let cmd = genCommand lc
  r <- S.run (S.fromText "docker") [ "run", "--rm", "-v"
                                   , "/tmp/eval:/home/eval/eval", "-m"
                                   , "512m", "--kernel-memory", "64m"
                                   , "--cpus=0.5", "--network=none", "evalbot"
                                   , "timeout", "5", "bash", "-c", cmd
                                   ]
  err <- S.lastStderr
  return $ r <> err
