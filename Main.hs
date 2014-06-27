{-# LANGUAGE  OverloadedStrings, FlexibleContexts #-}

import Adaptor.SCGI
import Junction.Git (revparse, lookupCommit, commitTreeEntry, lookupBlob
  ,openGitRepository, blobEntryOid)

import qualified Data.ByteString.Char8 as B (ByteString, concat, pack, unpack, readFile, null)
import Data.String (fromString)
import qualified Data.Text as T (pack)
import Network.URI (unEscapeString)
import Network.Mime (defaultMimeLookup)
import Text.Regex (mkRegex, matchRegex)
import Text.Regex.Posix ((=~))
import Data.Char (isSpace)
import Data.Maybe (fromJust, fromMaybe, isNothing)
import Data.List (isSuffixOf, isPrefixOf)
import System.Environment (getArgs)
import Control.Concurrent (newEmptyMVar, takeMVar, putMVar, MVar, forkOS)
import Database.PostgreSQL.LibPQ
import Control.Monad (forever)
import Control.Exception (catch, SomeException)
import Control.Applicative ((<$>))

import Data.IORef

import Debug.Trace

(-/-) :: Monad m => m (Either a b) -> (b -> m (Either a c)) -> m (Either a c)
(-/-) x y = x >>= either (return . Left) y

main :: IO ()
main = do
  args <- getArgs
  let sport:rbase:dtree:err404:dbase:_ = args
      port = read sport :: Int
  db <- databaser dbase
  runSCGI 10 port (git_main rbase dtree err404 db)

readGit :: String -> String -> String -> IO (Either String B.ByteString)
readGit path vers filnam =
  if vers == "-" then rf ( path ++ filnam ) else
  catch (openGitRepository path -/- getBlob) (\x -> return (Left (show (x :: SomeException)  ) ) )
  where -- vers = case vursion of { Nothing -> ""; Just x -> x }
        getBlob repo = revparse repo vers -/- lookupCommit repo
                         -/- (\aa -> commitTreeEntry repo aa (fromString filnam))
                         -/- (lookupBlob repo . blobEntryOid)
      -- a <- trace v $ resolveReference (fromString treeish)
        rf x = catch ( Right <$> B.readFile x ) (\y -> return $ Left (show (y::SomeException)) )

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
     in if B.null during then return r else do
          rfx <- rgx (tail (B.unpack (head fnams)))
          substitute (B.concat (case rfx of { Left a -> [before, "\r\n\r\n*** ",B.pack a," ***\r\n\r\n", after]; Right rfz -> [before, rfz, after] })) rgx

getVursionFromSession :: SessionContext -> B.ByteString
getVursionFromSession (SessionContext a) = let uid:cmp:rol:vurs:intercom:_ = a in vurs

mimeType :: String -> String
mimeType = B.unpack . defaultMimeLookup . T.pack

isNonBlank :: String -> Bool
isNonBlank x = let lbr = dropWhile isSpace x in not (null lbr || '#' == head lbr)

insRegex :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
insRegex g s z = let (before, during, after) = s =~ g
                  in if B.null during then s else B.concat [before, during, z, after]

needsLogin :: String -> Bool
needsLogin s = (isPrefixOf "app/" s || isPrefixOf "amber/" s ) && isSuffixOf "/index.html" s

-- could use DOCUMENT_ROOT for the repobase
git_main :: String -> String -> String -> ReqRsp DbRequest SessionContext -> CGI -> IO ()
git_main repobase dtree err404 db cgir = do
  hdrs <- cgiGetHeaders cgir
  let repo = if last repobase == '/' then repobase else repobase++"/"
      uux = unEscapeString (fromJust $ lookup "PATH_INFO" hdrs)
      uu = if last uux == '/' then tail uux ++ "index.html" else tail uux
      qryString qsx = matchRegex (mkRegex "(^|[&?])vursion=([^&]*)") (unEscapeString qsx)
      nvx = maybe Nothing qryString (lookup "QUERY_STRING" hdrs)
      cookies = case lookup "HTTP_COOKIE" hdrs of { Nothing -> []; Just x -> readCookies x }
      (treeishx,setcookie) = case nvx of
                 Nothing -> (lookup "vursion" cookies, Nothing)
                 Just [_,b] -> (Just b,
                                case b of { "" -> Just "vursion=deleted; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT";
                                            _ -> Just ("vursion="++b++"; path=/") } )
                 Just _ -> error "nvx cannot match this"
      jsess = fromMaybe "" (lookup "JSESSIONID" cookies)
      treeish = case treeishx of {Nothing -> dtree; Just "" -> dtree; Just x -> x }
  sess <- if "index.html" `isSuffixOf` uu then makeRequest db (jsess, treeish)
                                         else return $ SessionContext ["","","","",""]
  putStrLn ("serving "++uu++ " -- " ++ show sess ++ "/" ++ show jsess ++ "/" ++ show treeish )

  case sess of
    DbError dberr -> do
      sendResponse cgir[("Status","503 Database error"),("Content-Type","text/html")]
      writeResponse cgir $ B.concat ["Database connection error: ", dberr]
    _ -> if noUser sess && needsLogin uu then sendRedirect cgir "/login/" else
      if not (noUser sess) && (null uu || uu == "index.html") then
        let SessionContext (_:_:rol:_) = sess
            ru = "/app/home/" ++ (if rol == "issuer" then "company" else "investor")
         in sendRedirect cgir ru
      else do
      let treeishfdb = getVursionFromSession sess
          rgit = readGit repo treeish
          mt = mimeType uu
          addGtm x = do
             a <- rgit "i/tags.inc"
             case a of
               Left y -> return x
               Right y -> return $ insRegex "<body[^>]*>" x y
          headers = ("Content-Type",mt) : case setcookie of { Nothing -> []; Just b -> [("Set-Cookie",b)] }
          doHtml body = sendResponse cgir (("Status","200 OK") : headers) >>
                       (if mt == "text/html" then substitute body rgit >>= addGtm >>= return . addSess (jsess, treeish) sess else return body) >>=
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

-----------------------------------------------------------------------------------------------
-- Session stuff
-----------------------------------------------------------------------------------------------

noUser :: SessionContext -> Bool
noUser (SessionContext a) = B.null (head a)

type ReqRsp a b =  MVar (a, MVar b)
type DbRequest = (String, String)
data SessionContext = SessionContext [B.ByteString] | DbError B.ByteString

instance Show SessionContext where
  show (SessionContext a) = let uid:cmp:rol:vurs:intercom:_ = a in
    if B.null uid then ""
    else (B.unpack . B.concat) ["<script>document.sessionState={'userid': " ,enstr uid ,
                            ",'company': " , enstr cmp , ",'role':", enstr rol ,",'intercom':",enstr intercom, "};</script>"]
    where enstr s = B.concat["'",s,"'"]
--  show _ = "/* SessionContext should never match this */"  -- this is an error and should never happen
  show (DbError a) = (B.unpack . B.concat) ["*** ",a, " ***"]

makeRequest :: (Show b, Show a) => ReqRsp a b -> a -> IO b
makeRequest x d = do
  a <- newEmptyMVar
  putMVar x (d,a)
  z <- takeMVar a
--  print (d,z)
  return z

-- | This function starts a thread which communicates with the database to retrieve session information
databaser :: String -> IO (ReqRsp DbRequest SessionContext)
databaser dbConn = do
  inbox <- newEmptyMVar
  idb <- connectdb (fromString dbConn)
  conn <- newIORef ( idb, fromString dbConn)
  _ <- forkOS $ forever $ do
    ( (req,vurs), rsp) <- takeMVar inbox -- get a request
    putMVar rsp =<< getsess conn req vurs
  return inbox

varchar :: Oid
varchar = Oid 1043

getsess :: IORef (Connection, B.ByteString) -> String -> String -> IO SessionContext
getsess iconn js vurs = do
  (conn, nret) <- readIORef iconn
  cc <- execParams conn "select * from session.check_session($1, $2)"
                        [Just (varchar, B.pack js, Text),
                         Just (varchar, B.pack vurs, Text) ]
                        Text
  -- print (js, vurs, cc)
  -- if cc is Nothing, I have to complain loudly?
  -- certainly if conn is nothing, I do
  cd <- if isNothing cc then do
           print ("Resetting database connection" :: String)
           nconn <- connectdb nret
           erm <- fmap (fromMaybe "") (errorMessage nconn)
           print erm
           writeIORef iconn (nconn, nret)
           execParams nconn "select * from session.check_session($1, $2)"
                        [Just (varchar, B.pack js, Text),
                         Just (varchar, B.pack vurs, Text) ]
                        Text
        else return cc
  (conn,_) <- readIORef iconn
  erm <- fmap (fromMaybe "") (errorMessage conn)
  case cd of
            Nothing -> do
              print (B.concat ["***** ====> ",erm])
              return $ DbError erm
            Just dbres -> do
                       rs <- resultStatus dbres -- should be TuplesOK
                       nt <- ntuples dbres
                       nf <- nfields dbres
                       -- print (rs, erm, nt, nf)
                       if nt == 0 then return $ SessionContext ["","","","",""]
                       else do
                          m <- mapM ( \y -> return . fromMaybe "" =<< getvalue dbres 0 y) [0..nf-1]
                          -- print m
                          return (SessionContext m)
addSess :: DbRequest -> SessionContext -> B.ByteString -> B.ByteString
addSess (jid,vurs) sess s = let (before, during, after) = s =~ ("<head>" :: B.ByteString)
                                 in if B.null during then s else B.concat [ before, during, B.pack $ show sess, after]
