{-# LANGUAGE OverloadedStrings #-}

module Adaptor.SCGI ( runSCGI, CGI(..), cgiGetHeaders, cgiGetBody,
    writeResponse, sendResponse, sendRedirect, doCGI
  ) where

import Preface

handler :: (CGI -> IO ()) -> QSem -> Socket -> IO ()
handler f qsem sockt = do
  waitQSem qsem
  (sock, _) <- sktAccept sockt
  _ <- forkIO $ do
      catch (doSCGI f sock) (\e -> hPutStrLn stderr $ "scgi: "++show (e::SomeException))
      signalQSem qsem
  handler f qsem sockt

-- | runSCGI threads port cgi-function (in the IO monad)
runSCGI :: Int -> Int -> (CGI -> IO () ) -> IO ()
runSCGI maxThreads port f = join (doListen port <$> handler f <$> newQSem maxThreads )

doSCGI :: ( CGI -> IO() ) -> Socket -> IO ()
doSCGI f sock = do
  a <- getSCGI sock
  f a
  writeResponse a (zilde :: ByteString)

-- | WARNING: this ignores non-digits
toInt :: String -> Int
toInt z = foldl (\x y -> 10*x+y) 0 (map digitToInt (filter isDigit z))

netstring :: NetData -> IO (ByteString, NetData)
netstring nd =
  let (lenx, rest) = strBrk (== ':') (ndBuf nd)
      lens = asString lenx
      len = toInt lens
   in do
        (res, ndxx) <- ndGet len nd { ndBuf = strDrop (1 :: Int) rest }
        (_,ndxxx) <- ndGet (1 :: Int) ndxx
        return (res,ndxxx)

ndGet :: Integral b => b -> NetData -> IO (ByteString, NetData)
ndGet x nd@(NetData sok buf) =
  if fromIntegral x <= strLen buf then let (bh, bt) = strSplitAt x buf in return (bh, nd { ndBuf = bt } )
  else do
    let y = fromIntegral x - fromIntegral (strLen buf) :: Int
    more <- sktRecv sok (max y 4096 )
    if strLen more > 0 then do
      (res, ndx) <-  ndGet y nd { ndBuf = more }
      return ( strCat [buf, res], ndx )
    else return (ndBuf nd, nd { ndBuf = zilde})

ndNew :: Socket -> IO NetData
ndNew sock = NetData sock <$> sktRecv sock 4096

-- | Given a socket, sparks a thread to read it, and returns an MVar which can retrieve the
-- CGI variables, and a channel which can retrieve the post body in 4k chunks
getSCGI :: Socket -> IO CGI
getSCGI sock = do
    eenv <- newEmptyMVar
    chan <- newChan
    _ <- forkIO $ do
        (input, ndx) <- netstring =<< ndNew sock
        let vars = headersx ( asString input)
            len = case lookup "CONTENT_LENGTH" vars of { Nothing -> 0; Just x -> toInt x }
        putMVar eenv vars
        recurse len ndx chan
    hdrs <- newEmptyMVar
    wchan <- newChan
    _ <- forkIO $ do
        hd <- takeMVar hdrs
        -- I could send the headers immediately and then wait for the body, OR insert into the output stream
        -- sendFully sock hdx
        writeBody wchan sock (genhdrs hd)
    return $ CGI eenv chan hdrs wchan
  where blksiz = 4096
        writeBody x k buf = do
              buf2 <- sendBlocks k buf (8192 :: Int)
              bb <- readChan x
              if strLen bb == (0 :: Integer) then sendBlocks k buf2 (-1 :: Integer) >> sClose k
              else writeBody x k (strCat [buf2, bb])
        recurse n nnd ch =
              if n <= 0 then writeChan ch zilde
              else do (b,nnd')<-ndGet (min blksiz n) nnd
                      writeChan ch b
                      recurse (n- blksiz) nnd' ch
        headersx = pairs . split
        pairs (x:y:xys) = (x, y) : pairs xys
        pairs _ = []
        split str = let (token, rest) = strBrk (== '\NUL') str
                     in if null rest then [token] else  token : split (tail rest)

genhdrs :: HTTPHeaders -> ByteString 
genhdrs hd =  strCat . intersperse "\r\n" $ [ asByteString (n++": "++v) | (n,v) <- hd] ++ [zilde,zilde]

sendRedirect :: CGI -> String -> IO ()
sendRedirect rsp loc = putMVar (cgiRspHeaders rsp) [("Status","302 Found"),("Location",loc)]

sendResponse :: CGI -> HTTPHeaders -> IO ()
sendResponse rsp = putMVar (cgiRspHeaders rsp)

writeResponse :: Stringy a => CGI -> a -> IO ()
writeResponse rsp = writeChan (cgiRspBody rsp) . asByteString

sendBlocks :: Integral b => Socket -> ByteString -> b -> IO ByteString
sendBlocks s bs n = if strLen bs < fromIntegral n then return bs else do
  sent <- sktSend s bs
  let res = strDrop sent bs
  if strLen res == 0 then return res else sendBlocks s res n

doListen :: Int -> (Socket -> IO ()) -> IO ()
doListen port loop = withSocketsDo $ bracket (listenOn (PortNumber (fromIntegral port))) sClose loop

-- defaultContentType :: String
-- defaultContentType = "text/html; charset=ISO-8859-1"

data NetData = NetData {
  _ndSocket :: Socket,
  ndBuf :: ByteString
} deriving (Show)

type CGIVars = [(String,String)]
type HTTPHeaders = [(String,String)]
type PostBody = ByteString

data CGI = CGI { cgiRqHeaders :: MVar CGIVars,  cgiRqBody :: Chan PostBody
                , cgiRspHeaders :: MVar HTTPHeaders, cgiRspBody :: Chan PostBody
               }

cgiGetHeaders :: CGI -> IO CGIVars
cgiGetHeaders = takeMVar . cgiRqHeaders

cgiGetBody :: CGI -> IO ByteString
cgiGetBody cgir = cgiGetBody' cgir ""
  where cgiGetBody' cgx sf = do
          x <- readChan (cgiRqBody cgx)
          if strNull x then return sf else cgiGetBody' cgx (strCat [sf, x])

doCGI :: ( CGI -> IO() ) -> IO ()
doCGI f = do
  a <- getCGI
  f a
  writeResponse a (zilde :: ByteString)

writeBlocks :: ByteString -> IO ()
writeBlocks = strPut

getCGI :: IO CGI
getCGI = do
    eenv <- newEmptyMVar
    chan <- newChan
    vars <- getEnvironment
    putMVar eenv vars

    hdrs <- newEmptyMVar
    wchan <- newChan
    _ <- forkIO $ do
        hd <- takeMVar hdrs
        -- I could send the headers immediately and then wait for the body, OR insert into the output stream
        -- sendFully sock hdx
        writeBody wchan (genhdrs hd)
    return $ CGI eenv chan hdrs wchan
  where -- blksiz = 4096
        writeBody x buf = do
              writeBlocks buf
              bb <- readChan x
              when (strLen bb > 0) (writeBody x bb)
        {-
        recurse n nnd ch = do
              if n <= (0 :: Int) then writeChan ch (zilde :: ByteString)
              else do (b,nnd')<-ndGet (min blksiz n) nnd
                      writeChan ch b
                      recurse (n- blksiz) nnd' ch
        -}