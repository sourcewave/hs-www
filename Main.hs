{-# LANGUAGE  OverloadedStrings, FlexibleContexts #-}

import Adaptor.SCGI

import qualified Data.ByteString.Char8 as B (ByteString, pack, unpack, readFile)
import Data.String ()
import qualified Data.Text as T (pack)
import Network.URI (unEscapeString)
import Network.Mime (defaultMimeLookup)
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.List (isSuffixOf)
import System.Environment (getArgs)
import Control.Exception (catch, SomeException)
import Control.Applicative ((<$>))

main :: IO ()
main = do
  args <- getArgs
  let sport:rbase:dtree:err404:_ = args
      port = read sport :: Int
  runSCGI 10 port (git_main rbase dtree err404)

readGit :: String -> String -> String -> IO (Either String B.ByteString)
readGit path _ filnam =
  rf ( path ++ filnam )
  where rf x = catch ( Right <$> B.readFile x ) (\y -> return $ Left (show (y::SomeException)) )

mimeType :: String -> String
mimeType = B.unpack . defaultMimeLookup . T.pack

isNonBlank :: String -> Bool
isNonBlank x = let lbr = dropWhile isSpace x in not (null lbr || '#' == head lbr)

-- could use DOCUMENT_ROOT for the repobase
git_main :: String -> String -> String -> CGI -> IO ()
git_main repobase dtree err404 cgir = do
  hdrs <- cgiGetHeaders cgir
  let repo = if last repobase == '/' then repobase else repobase++"/"
      uux = unEscapeString (fromJust $ lookup "PATH_INFO" hdrs)
      uu = if last uux == '/' then tail uux ++ "index.html" else tail uux
      treeish = dtree
  putStrLn ("serving "++uu++ " -- " ++ show treeish )

  do
      let rgit = readGit repo "-"
          mt = mimeType uu
          headersa = ("Content-Type",mt) : []
          headers = [("X-Vursion",treeish)] ++ headersa
          doHtml body = sendResponse cgir (("Status","200 OK") : headers) >>
                        return body >>=
                       writeResponse cgir
          doCat body = do
              mm <- mapM rgit (filter isNonBlank (lines ( B.unpack body) ))
              let mmt = mimeType (take (length uu - 4) uu)
              sendResponse cgir ([("Status","200 OK"),("Content-Type",mmt)]++tail headers)
              mapM_ (\z -> case z of { Right a -> writeResponse cgir a; Left b -> putStrLn b >> writeResponse cgir (B.pack ("\r\n/* *** "++b++" *** */\r\n")) } ) mm
          fmtErr err = B.pack ("failed to read version "++ show treeish ++" of: " ++ uu ++ "\r\n\r\n" ++ err)

          sendErr err = do
            sendResponse cgir [("Status","404 Not found"),("Content-Type","text/html")]
            errm <- rgit err404
            writeResponse cgir $ case errm of { Left _ -> fmtErr err; Right x -> x }
      rgit uu >>= either sendErr ( if ".cat" `isSuffixOf` uu then doCat else doHtml )
