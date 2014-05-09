{-# LANGUAGE  OverloadedStrings, FlexibleContexts #-}

import WWW.SCGI
import Bindings.Git (revparse, lookupCommit, commitTreeEntry, lookupBlob
  ,openGitRepository, blobEntryOid)

import qualified Data.ByteString.Char8 as B (ByteString, concat, pack, unpack, readFile, null)
import Data.String (fromString)
import qualified Data.Text as T (pack)
import Network.URI (unEscapeString)
import Network.Mime (defaultMimeLookup)
import Text.Regex (mkRegex, matchRegex)
import Text.Regex.Posix ((=~))
import Data.Char (isSpace)
import Data.Maybe (fromJust)
import Data.List (isSuffixOf)
import Data.Either (either)
import System.Environment (getArgs)

import Control.Exception (catch, SomeException)

import Debug.Trace

(-/-) :: Monad m => m (Either a b) -> (b -> m (Either a c)) -> m (Either a c)
(-/-) x y = x >>= either (return . Left) y

main :: IO ()
main = do
  args <- getArgs
  let port = (read (head args)) :: Int
      rbase = (head . tail) args
  runSCGI 1 port (git_main [("RepoBase",rbase)])

doer :: String -> String -> String -> IO (Either String B.ByteString)
doer path treeish filnam = openGitRepository path -/- getBlob
  where getBlob repo = revparse repo treeish -/- lookupCommit repo
                         -/- (\aa -> commitTreeEntry repo aa (fromString filnam))
                         -/- ((lookupBlob repo) . blobEntryOid)
      -- a <- trace v $ resolveReference (fromString treeish)

readCookies :: String -> [(String,String)]
readCookies s = 
    let (xs,ys) = break (=='=') (dropWhile isSpace s)
        (zs,ws) = break (==';') (dropWhile isSpace (drop 1 ys))
     in if null xs then [] else (xs,zs):readCookies (drop 1 ws)

-- substitute readGit for rf  so that the includes resolve properly
substitute :: B.ByteString -> (String -> IO (Either String B.ByteString)) -> IO B.ByteString
substitute r rgx =
     let rx = "<!--\\{\\{(.*)\\}\\}-->" :: B.ByteString
         (before, during, after, fnams) = r =~ rx :: (B.ByteString, B.ByteString, B.ByteString, [B.ByteString])
     in if (B.null during) then return r else do
          rfx <- rgx (tail (B.unpack (head fnams)))
          substitute (B.concat (case rfx of { Left a -> [before, "\r\n\r\n*** ",(B.pack a)," ***\r\n\r\n", after]; Right rfz -> [before, rfz, after] })) rgx

rf :: String -> IO (Either String B.ByteString)  
rf x = catch ( fmap Right $ B.readFile x ) (\y -> return $ Left (show (y::SomeException)) )

mimeType :: String -> String
mimeType = B.unpack . defaultMimeLookup . T.pack

readGit :: String -> String -> String -> IO (Either String B.ByteString)
readGit url vursion repo = case vursion of 
         "" -> rf ( repo ++ url)
         _ ->  doer repo vursion url 

isNonBlank :: String -> Bool
isNonBlank x = let lbr = dropWhile isSpace x in not (null lbr || '#' == head lbr)

-- could use DOCUMENT_ROOT for the repobase 
git_main :: [(String,String)] -> CGI -> IO ()
git_main cfg cgir = do
  hdrs <- cgiGetHeaders cgir
  let repox = fromJust (lookup "RepoBase" cfg)
      repo = if last repox == '/' then repox else repox++"/"
      
      uux = unEscapeString (fromJust $ lookup "PATH_INFO" hdrs)
      uu = if last uux == '/' then tail uux ++ "index.html" else tail uux
      nvx = case lookup "QUERY_STRING" hdrs of 
                 Nothing -> Nothing
                 Just qsx -> matchRegex (mkRegex "(^|[&?])vursion=([^&]*)") (unEscapeString qsx)
      cookies = case lookup "HTTP_COOKIE" hdrs of { Nothing -> []; Just x -> readCookies x }
      (treeish,setcookie) = case nvx of
                 Nothing -> (lookup "vursion" cookies, Nothing)
                 Just [_,b] -> (Just b, 
                                case b of { "" -> Just ("vursion=deleted; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT");
                                            _ -> Just ("vursion="++b++"; path=/") } )
                 Just _ -> error "nvx cannot match this"
      rgit = \f -> readGit f (case treeish of {Nothing -> ""; Just x -> x}) repo  
      mt = mimeType uu 
      headers = [("Content-Type",mt)] ++ case setcookie of { Nothing -> []; Just b -> [("Set-Cookie",b)] }
  -- putStrLn ("serving "++uu)
  zxy <- rgit uu
  case zxy of 
          Left err -> do
             sendResponse cgir [("Status","404 Not found"),("Content-Type","text/plain")]
             writeResponse cgir (B.pack ("failed to read version "++(show treeish) ++" of: " ++ uu ++ "\r\n\r\n" ++ err)  )
             return ()
          Right zz -> do
            if isSuffixOf ".cat" uu then do
              mm <- mapM rgit (filter isNonBlank (lines ( B.unpack zz) ))
              let mmt = mimeType (take (length uu - 4) uu)
              sendResponse cgir ([("Status","200 OK"),("Content-Type",mmt)]++tail headers)
              mapM_ (\z -> case z of { Right a -> writeResponse cgir a; Left b -> putStrLn b >> writeResponse cgir (B.pack ("\r\n/* *** "++b++" *** */\r\n")) } ) mm
            else do 
              text <- if mt == "text/html" then substitute zz rgit else return zz
              sendResponse cgir ([("Status","200 OK")]++headers)
              writeResponse cgir text
              
