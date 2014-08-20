{-# LANGUAGE  OverloadedStrings, FlexibleContexts #-}

import Preface
import Adaptor.SCGI
import Junction.Git (revparse, lookupCommit, commitTreeEntry, lookupBlob
  ,openGitRepository, blobEntryOid)
import qualified PostgreSQL as PG

(-/-) :: Monad m => m (Either a b) -> (b -> m (Either a c)) -> m (Either a c)
(-/-) x y = x >>= either (return . Left) y

main :: IO ()
main = do
  args <- getArgs
  let sport:rbase:dtree:err404:dbase:_ = args
      port = read sport :: Int

  inbox <- newEmptyMVar
  conn <- newIORef ( undefined, dbase)
  reconnect conn
  _ <- forkOS $ forever $ do
    ( (req,vurs), rsp) <- takeMVar inbox -- get a request
    putMVar rsp =<< getsess conn req vurs

  runSCGI 10 port (git_main rbase dtree err404 inbox)

readGit :: String -> String -> String -> IO (Either String ByteString)
readGit path vers filnam =
  if vers == "-" then rf ( path ++ filnam ) else
  catch (openGitRepository path -/- getBlob) (\x -> return (Left (show (x :: SomeException)  ) ) )
  where -- vers = case vursion of { Nothing -> ""; Just x -> x }
        getBlob repo = revparse repo vers -/- lookupCommit repo
                         -/- (\aa -> commitTreeEntry repo aa (fromString filnam))
                         -/- (lookupBlob repo . blobEntryOid)
      -- a <- trace v $ resolveReference (fromString treeish)
        rf x = catch ( Right <$> byteReadFile x ) (\y -> return $ Left (show (y::SomeException)) )

readCookies :: String -> [(String,String)]
readCookies s =
    let (xs,ys) = break (=='=') (dropWhile isSpace s)
        (zs,ws) = break (==';') (dropWhile isSpace (drop 1 ys))
     in if null xs then [] else (xs,zs):readCookies (drop 1 ws)

-- substitute readGit for rf  so that the includes resolve properly
substitute :: ByteString -> (String -> IO (Either String ByteString)) -> IO ByteString
substitute r rgx =
     let rx = "<!--\\{\\{(.*)\\}\\}-->" :: ByteString
         (before, during, after, fnams) = r =~ rx :: (ByteString, ByteString, ByteString, [ByteString])
     in if strNull during then return r else do
          rfx <- rgx (tail (asString (head fnams)))
          substitute (strCat (case rfx of { Left a -> [before, "\r\n\r\n*** ",asByteString a," ***\r\n\r\n", after]; Right rfz -> [before, rfz, after] })) rgx

getVursionFromSession :: SessionContext -> ByteString
getVursionFromSession (SessionContext a) = let uid:cmp:rol:vurs:intercom:_ = a in vurs

mimeType :: String -> String
mimeType = asString . defaultMimeLookup . asText

isNonBlank :: String -> Bool
isNonBlank x = let lbr = dropWhile isSpace x in not (strNull lbr || '#' == head lbr)

insRegex :: ByteString -> ByteString -> ByteString -> ByteString
insRegex g s z = let (before, during, after) = s =~ g
                  in if strNull during then s else strCat [before, during, z, after]

needsLogin :: String -> Bool
needsLogin s = (isPrefixOf "app/" s || isPrefixOf "amber/" s ) && isSuffixOf "/index.html" s

-- could use DOCUMENT_ROOT for the repobase
git_main :: String -> String -> String -> ReqRsp DbRequest SessionContext -> CGI -> IO ()
git_main repobase dtree err404 db cgir = do
  hdrs <- cgiGetHeaders cgir
  let repo = if last repobase == '/' then repobase else repobase++"/"
      uux = unEscapeString (fromJust $ lookup "PATH_INFO" hdrs)
      uu = if last uux == '/' then tail uux ++ "index.html" else tail uux
      qryString :: String -> Maybe String
      qryString qsx = let z = ((unEscapeString qsx) =~ ("(^|[&?])vursion=([^&]*)"::String) :: [[String]] )
                      in if null z then Nothing else (let (a:b:c:_) = head z in Just c)
      nvx = maybe Nothing qryString (lookup "QUERY_STRING" hdrs)
      cookies = case lookup "HTTP_COOKIE" hdrs of { Nothing -> []; Just x -> readCookies x }
      (treeishx,setcookie) = case nvx of
                 Nothing -> (lookup "vursion" cookies, Nothing)
                 Just b -> (Just b,
                                case b of { "" -> Just "vursion=deleted; path=/; expires=Thu, 01-Jan-1970 00:00:00 GMT";
                                            _ -> Just ("vursion="++b++"; path=/") } )
                 Just _ -> error "nvx cannot match this"
      jsess = fromMaybe "" (lookup "JSESSIONID" cookies)
      treeish = case treeishx of {Nothing -> dtree; Just "" -> dtree; Just x -> x }
  putStrLn ("pre-db lookup" ++ show (jsess, treeish))
  sess <- if "index.html" `isSuffixOf` uu then makeRequest db (jsess, treeish)
                                         else return $ SessionContext ["","","","",""]
  putStrLn ("serving "++uu++ " -- " ++ show sess ++ "/" ++ show jsess ++ "/" ++ show treeish )

  case sess of
    DbError dberr -> do
      sendResponse cgir[("Status","503 Database error"),("Content-Type","text/html")]
      writeResponse cgir $ strCat ["Database connection error: ", dberr]
    _ -> if noUser sess && needsLogin uu then sendRedirect cgir "/login/" else
      if not (noUser sess) && (strNull uu || uu == "index.html") then
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
          headers = ("Content-Type",mt) : ("X-Vursion", asString treeishfdb) : case setcookie of { Nothing -> []; Just b -> [("Set-Cookie",b)] }
          doHtml body = sendResponse cgir (("Status","200 OK") : headers) >>
                       (if mt == "text/html" then substitute body rgit >>= addGtm >>= return . addSess (jsess, treeish) sess else return body) >>=
                       writeResponse cgir
          doCat body = do
              mm <- mapM rgit (filter isNonBlank (lines ( asString body) ))
              let mmt = mimeType (take (length uu - 4) uu)
              sendResponse cgir ([("Status","200 OK"),("Content-Type",mmt)]++tail headers)
              mapM_ (\z -> case z of { Right a -> writeResponse cgir a; Left b -> putStrLn b >> writeResponse cgir (asByteString ("\r\n/* *** "++b++" *** */\r\n")) } ) mm
          fmtErr err = asByteString ("failed to read version "++ show treeish ++" of: " ++ uu ++ "\r\n\r\n" ++ err)

          sendErr err = do
            sendResponse cgir [("Status","404 Not found"),("Content-Type","text/html")]
            errm <- rgit err404
            writeResponse cgir $ case errm of { Left _ -> fmtErr err; Right x -> x }
      rgit uu >>= either sendErr ( if ".cat" `isSuffixOf` uu then doCat else doHtml )

-----------------------------------------------------------------------------------------------
-- Session stuff
-----------------------------------------------------------------------------------------------

noUser :: SessionContext -> Bool
noUser (SessionContext a) = strNull (head a)

type ReqRsp a b =  MVar (a, MVar b)
type DbRequest = (String, String)
data SessionContext = SessionContext [ByteString] | DbError ByteString

instance Show SessionContext where
  show (SessionContext a) = let uid:cmp:rol:vurs:intercom:_ = a in
    if strNull uid then ""
    else (asString . strCat) ["<script>document.sessionState={'userid': " ,enstr uid ,
                            ",'company': " , enstr cmp , ",'role':", enstr rol ,",'intercom':",enstr intercom, "};</script>"]
    where enstr s = strCat["'",s,"'"]
--  show _ = "/* SessionContext should never match this */"  -- this is an error and should never happen
  show (DbError a) = (asString . strCat) ["*** ",a, " ***"]

makeRequest :: (Show b, Show a) => ReqRsp a b -> a -> IO b
makeRequest x d = do
  a <- newEmptyMVar
  putMVar x (d,a)
  z <- takeMVar a
--  print (d,z)
  return z

-- | This function starts a thread which communicates with the database to retrieve session information
-- databaser :: String -> IO (ReqRsp DbRequest SessionContext)

-- varchar :: Oid
-- varchar = Oid 1043

reconnect :: IORef (PG.Postgres, String) -> IO PG.Postgres
reconnect ior = do
  (_, fsdb) <- readIORef ior
  idb <- PG.connectToDb fsdb
  -- what happens if there is a connection error?
  -- erm <- fmap (fromMaybe "") (PG.errorMessage idb)
  -- print erm
  
  -- what to do if there was an error?
  PG.sendQuery idb (PG.Parse "q1" "select * from session.check_session($1, $2)" [1043, 1043])
  writeIORef ior (idb, fsdb)
  return idb

doGetsess conn js vurs = do
  PG.sendQuery conn (PG.Bind "" "q1" [Just (asByteString js), Just (asByteString vurs)] )
  PG.sendQuery conn (PG.Execute "" 1)
  PG.sendQuery conn (PG.ClosePortal "")
  cc <- PG.doQuery conn PG.Sync
  print cc
  return cc

getsess :: IORef (PG.Postgres, String) -> String -> String -> IO SessionContext
getsess iconn js vurs = do
  (conn, nret) <- readIORef iconn
  cc <- doGetsess conn js vurs

-- I could get an EndSession (or maybe an Error? )
  case cc of
    PG.EndSession -> do
      print ("Resetting database connection" :: String)
      reconnect iconn
      -- is this a bottomless recursion ?
      getsess iconn js vurs

    PG.ResultSet rd fdr fz ->
      if null fdr then return $ SessionContext [ "","","","",""]
      else let dra = head fdr in return (SessionContext (map (maybe "" id) dra))

    _ -> do
      print ("unkown response from database: " ++ show cc)
      error (show cc)
      
  -- print (js, vurs, cc)
  -- if cc is Nothing, I have to complain loudly?
  -- certainly if conn is nothing, I do
{-
  cd <- if isNothing cc then do
           print ("Resetting database connection" :: String)
           nconn <- PG.connectToDb nret

           erm <- fmap (fromMaybe "") (errorMessage nconn)
           print erm
           writeIORef iconn (nconn, nret)
           execParams nconn "select * from session.check_session($1, $2)"
                        [Just (varchar, B.pack js, Text),
                         Just (varchar, B.pack vurs, Text) ]
                        Text
        else return cc
  (conn,_) <- readIORef iconn
  -}

-- WHAT about errors
--  erm <- fmap (fromMaybe "") (errorMessage conn)
 {-
  let cd = cc
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
  -}
addSess :: DbRequest -> SessionContext -> ByteString -> ByteString
addSess (jid,vurs) sess s = let (before, during, after) = s =~ ("<head>" :: ByteString)
                                 in if strNull during then s else strCat [ before, during, asByteString $ show sess, after]
