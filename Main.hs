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
import Data.List (isSuffixOf, isPrefixOf)
import System.Environment (getArgs)
import Control.Concurrent (newEmptyMVar, takeMVar, putMVar, MVar(..), forkOS)
import Database.PostgreSQL.LibPQ
import Control.Monad (forever)
import Control.Exception (catch, SomeException)

import Debug.Trace

(-/-) :: Monad m => m (Either a b) -> (b -> m (Either a c)) -> m (Either a c)
(-/-) x y = x >>= either (return . Left) y

main :: IO ()
main = do
  args <- getArgs
  let sport:rbase:dtree:dbase:_ = args
      port = (read sport) :: Int
  db <- databaser dbase
  runSCGI 10 port (git_main rbase dtree db)

readGit :: String -> String -> String -> IO (Either String B.ByteString)
readGit path vers filnam = 
--  if null vers then rf ( path ++ filnam ) else
  catch (openGitRepository path -/- getBlob) (\x -> return (Left (show (x :: SomeException)  ) ) )
  where -- vers = case vursion of { Nothing -> ""; Just x -> x }
        getBlob repo = revparse repo vers -/- lookupCommit repo
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

getVursionFromSession :: SessionContext -> B.ByteString
getVursionFromSession (SessionContext a) = let uid:cmp:rol:vurs:intercom:_ = a in vurs

-- rf :: String -> IO (Either String B.ByteString)  
-- rf x = catch ( fmap Right $ B.readFile x ) (\y -> return $ Left (show (y::SomeException)) )

mimeType :: String -> String
mimeType = B.unpack . defaultMimeLookup . T.pack

isNonBlank :: String -> Bool
isNonBlank x = let lbr = dropWhile isSpace x in not (null lbr || '#' == head lbr)

insRegex :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString
insRegex g s z = let (before, during, after) = s =~ g
                  in if B.null during then s else B.concat [before, during, z, after]

needsLogin :: String -> Bool
needsLogin s = isPrefixOf "app/" s && isSuffixOf "/index.html" s 

-- could use DOCUMENT_ROOT for the repobase 
git_main :: String -> String -> ReqRsp DbRequest SessionContext -> CGI -> IO ()
git_main repobase dtree db cgir = do
  hdrs <- cgiGetHeaders cgir
  let repo = if last repobase == '/' then repobase else repobase++"/"
      uux = unEscapeString (fromJust $ lookup "PATH_INFO" hdrs)
      uu = if last uux == '/' then tail uux ++ "index.html" else tail uux
      qryString qsx = matchRegex (mkRegex "(^|[&?])vursion=([^&]*)") (unEscapeString qsx)
      nvx = maybe Nothing qryString (lookup "QUERY_STRING" hdrs)
      cookies = case lookup "HTTP_COOKIE" hdrs of { Nothing -> []; Just x -> readCookies x }
      (treeish,setcookie) = case nvx of
                 Nothing -> (lookup "vursion" cookies, Nothing)
                 Just [_,b] -> (Just b, 
                                case b of { "" -> Just ("vursion=deleted; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT");
                                            _ -> Just ("vursion="++b++"; path=/") } )
                 Just _ -> error "nvx cannot match this"
      jsess = case (lookup "JSESSIONID" cookies) of {Nothing -> ""; Just x -> x}
      
  sess <- if isSuffixOf "/index.html" uu then makeRequest db (jsess, treeish)
                                         else return $ SessionContext ["","","","",""]
  putStrLn ("serving "++uu++ " -- " ++ show sess ++ "/" ++ show jsess ++ "/" ++ show treeish )
                                         
  if noUser sess && needsLogin uu then sendRedirect cgir "/login/" else do
    let treeishfdb = getVursionFromSession sess
        rgit = readGit repo (case treeish of { Nothing -> dtree; Just x -> x })
        mt = mimeType uu 
        addGtm x = do 
           a <- rgit "i/tags.inc"
           case a of
               Left y -> return x
               Right y -> return $ insRegex "<body[^>]*>" x y
        headers = [("Content-Type",mt)] ++ case setcookie of { Nothing -> []; Just b -> [("Set-Cookie",b)] }
        doHtml body = sendResponse cgir ([("Status","200 OK")]++headers) >>
                       (if mt == "text/html" then substitute body rgit >>= addGtm >>= return . addSess (jsess, treeish) sess else return body) >>=
                       writeResponse cgir
        doCat body = do
              mm <- mapM rgit (filter isNonBlank (lines ( B.unpack body) ))
              let mmt = mimeType (take (length uu - 4) uu)
              sendResponse cgir ([("Status","200 OK"),("Content-Type",mmt)]++tail headers)
              mapM_ (\z -> case z of { Right a -> writeResponse cgir a; Left b -> putStrLn b >> writeResponse cgir (B.pack ("\r\n/* *** "++b++" *** */\r\n")) } ) mm
        fmtErr err = (B.pack ("failed to read version "++(show treeish) ++" of: " ++ uu ++ "\r\n\r\n" ++ err)  )
        sendErr err = sendResponse cgir [("Status","404 Not found"),("Content-Type","text/plain")] >>
                      writeResponse cgir  (fmtErr err)
    rgit uu >>= either sendErr ( if isSuffixOf ".cat" uu then doCat else doHtml )

-----------------------------------------------------------------------------------------------
-- Session stuff
-----------------------------------------------------------------------------------------------

noUser (SessionContext a) = B.null (head a)

type ReqRsp a b =  MVar (a, MVar b)
type DbRequest = (String, Maybe String)
newtype SessionContext = SessionContext [B.ByteString]

instance Show SessionContext where
  show (SessionContext a) = let uid:cmp:rol:vurs:intercom:_ = a in
    if B.null uid then ""
    else (B.unpack . B.concat) ["<script>document.sessionState={'userid': " ,enstr uid ,
                            ",'company': " , enstr cmp , ",'role':", enstr rol ,",'intercom':",enstr intercom, "};</script>"]
    where enstr s = B.concat["'",s,"'"] 
--  show _ = "/* SessionContext should never match this */"  -- this is an error and should never happen

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
  conn <- connectdb (fromString dbConn)
  _ <- forkOS $ forever $ do
    ( (req,vurs), rsp) <- takeMVar inbox -- get a request
    putMVar rsp =<< getsess conn req vurs
  return inbox

varchar = Oid 1043

getsess :: Connection -> String -> Maybe String -> IO SessionContext
getsess conn js vurs = do
  cc <- execParams conn "select * from session.check_session($1, $2)"
                        [Just (varchar, (B.pack js), Text), 
                         ( maybe Nothing (\x -> Just (varchar, (B.pack x), Text)) vurs ) ]
                        Text
  -- if cc is Nothing, I have to complain loudly?
  -- certainly if conn is nothing, I do
  case cc of 
            Nothing -> return $ SessionContext ["","","","",""]
            Just dbres -> do 
                       rs <- resultStatus dbres -- should be TuplesOK
                       erm <- errorMessage conn
                       nt <- ntuples dbres
                       nf <- nfields dbres
                       if nt == 0 then return $ SessionContext ["","","","",""]
                       else fmap SessionContext $ mapM ( \y -> return . maybe "" id =<< getvalue dbres 0 y) [0..nf-1]

addSess :: DbRequest -> SessionContext -> B.ByteString -> B.ByteString
addSess (jid,vurs) sess s = let (before, during, after) = s =~ ("<head>" :: B.ByteString)
                                 in if B.null during then s else B.concat [ before, during, (B.pack $ show sess), after]
