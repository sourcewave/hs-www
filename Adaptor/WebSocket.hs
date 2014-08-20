{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module Adaptor.WebSocket where

import Preface

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Base64 as B64

import qualified Crypto.Hash.SHA1 as SHA1
import Data.Bits ((.|.), (.&.), xor)


import System.Entropy

hashKey :: ByteString -> ByteString
hashKey = SHA1.hash . (flip strCat "258EAFA5-E914-47DA-95CA-C5AB0DC85B11")

encodeFrame :: ByteString -> FrameType -> ByteString -> ByteString
encodeFrame mask ft f = strCat [packBytes [byte0, byte1], len , mask, if strNull mask then f else maskPayload mask f]
  where
    byte0  = fromIntegral $ fin .|. rsv1 .|. rsv2 .|. rsv3 .|. opcode
    fin    = 0x80 -- if frameFin f  then 0x80 else 0x00
    rsv1   = 0x00 -- if frameRsv1 f then 0x40 else 0x00
    rsv2   = 0x00 -- if frameRsv2 f then 0x20 else 0x00
    rsv3   = 0x00 -- if frameRsv3 f then 0x10 else 0x00
    opcode = fromEnum ft 
    maskflag = if strNull mask then 0x00 else 0x80 :: Int
    len'  = fromIntegral ( strLen f ) :: Int
    (lenflag, len)
        | len' < 126     = (fromIntegral len', zilde)
        | len' < 0x10000 = (126, (BL.toStrict . runPut) (putWord16be (fromIntegral len')))
        | otherwise      = (127, (BL.toStrict . runPut) (putWord64be (fromIntegral len')))
    byte1 = fromIntegral (maskflag .|. lenflag)

data FrameType = ContinuationFrame | TextFrame | BinaryFrame | CloseFrame | PingFrame | PongFrame
    | UnknownFrame deriving (Show, Eq)

type Mask = ByteString

data WSFrame = Frame Bool (Bool,Bool,Bool) FrameType ByteString deriving (Show)

instance Enum FrameType where
  fromEnum x = case x of
        ContinuationFrame -> 0x00
        TextFrame         -> 0x01
        BinaryFrame       -> 0x02
        CloseFrame        -> 0x08
        PingFrame         -> 0x09
        PongFrame         -> 0x0a
        UnknownFrame      -> 0xff
  toEnum x = case x of
        0x00 -> ContinuationFrame
        0x01 -> TextFrame
        0x02 -> BinaryFrame
        0x08 -> CloseFrame
        0x09 -> PingFrame
        0x0a -> PongFrame
        _ -> UnknownFrame
        
maskPayload :: ByteString -> ByteString -> ByteString
maskPayload mask pl = traceShow (mask, pl) $ snd ( B.mapAccumL f 0 pl)
  where f i c = traceShow ("mask",i,c) $ ( (i+1) `mod` strLen mask,  c `xor` nthByte mask i)


--------------------------------------------------------------------------------
-- | Parse a frame
decodeFrame :: ByteString -> (Maybe WSFrame, ByteString)
decodeFrame z =
    let bl = strLen z
        byte0 = nthByte z 0
        byte1 = nthByte z 1
        fin    = byte0 .&. 0x80 == 0x80
        rsv1   = byte0 .&. 0x40 == 0x40
        rsv2   = byte0 .&. 0x20 == 0x20
        rsv3   = byte0 .&. 0x10 == 0x10
        opcode = byte0 .&. 0x0f
        ft = (toEnum . fromIntegral) opcode
        mask = byte1 .&. 0x80 == 0x80
        lenflag = fromIntegral (byte1 .&. 0x7f) :: Int
        bof = strDrop 2 z
        (mdl, len) = case lenflag of
          126 -> if bl < 4 then (2,-1) else (2, fromIntegral $ runGet getWord16be (BL.fromStrict (strTake 2 bof)))
          127 -> if bl < 10 then (8, -1) else (8, fromIntegral $ runGet getWord64be (BL.fromStrict (strTake 8 bof)))
          _   -> (0, lenflag)
        mdlm = if mask then mdl+4 else mdl
        masker = if mask then maskPayload (strTake 4 (strDrop mdl bof)) else id
        pl = strDrop mdlm bof
        chunk = strTake (fromIntegral len) pl -- in theory this could be a chunk greater than 2^31 in length
        leftover = strDrop (fromIntegral len) pl
     in traceShow ("voila ", if bl < 2 then show bl else show (bl, ft, mdl, mdlm, len)) $
           if bl < 2 || len < 0 || bl < mdlm + len then (Nothing, z)
           else (Just $ Frame fin (rsv1, rsv2, rsv3) ft (masker chunk), leftover)
--     return $ Frame fin rsv1 rsv2 rsv3 ft (masker $ BL.fromChunks chunks)

connectTo :: String -> Int -> String -> (WebSocket -> IO ()) -> IO () -- host port path headers 
connectTo host port path app = do
    -- Create and connect socket
    let hints = S.defaultHints {S.addrFamily = S.AF_INET, S.addrSocketType = S.Stream}
    addrInfos <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
    sock      <- S.socket S.AF_INET S.Stream S.defaultProtocol

    -- Connect WebSocket and run client
    finally (sktConnect sock (S.addrAddress $ head addrInfos) >>
             doClient sock host path app)
            (sClose sock)

doClient :: Socket -> String -> String -> (WebSocket -> IO ()) -> IO ()
doClient skt host path app = do
    -- Create the request and send it
    key <- B64.encode `liftM` getEntropy 16
    let headers = [("Host", host)
         , ("Connection", "Upgrade")
         , ("Upgrade", "websocket")
         , ("Sec-WebSocket-Key", byteStringToString key)
         , ("Sec-WebSocket-Version", "13")
         ]
        request = concat $ concat [
                    ["GET ", path , " HTTP/1.1\r\n"],
                    concat $ map (\(k,v) -> [k,": ",v,"\r\n"] ) headers,
                    ["\r\n"]]
    ws <- getWS skt (stringToByteString request)
    cct <- takeMVar (wsRspHeaders ws)
    print cct
    app ws
{-    
    response     <- Streams.parseFromStream decodeResponseHead sIn
    -- Note that we pattern match to evaluate the result here
    Response _ _ <- return $ finishResponse protocol request response
    mIn          <- decodeMessages sIn
    mOut         <- encodeMessages ClientConnection bOut
    app 
  where
    bHost    = T.encodeUtf8 $ T.pack host
    bPath    = T.encodeUtf8 $ T.pack path
-}
    
type HTTPHeaders = [(String,String)]
type PostBody = ByteString

data Message = Close | Text String | Binary ByteString deriving (Show)

-- ---------------------------
-- Copied from SCGI (could be collapsed)
data WebSocket = WebSocket { wsSend :: Chan Message
                             , wsRspHeaders :: MVar HTTPHeaders, wsRecv :: Chan WSFrame
                           }

class (Show a) => WebSocketMessage a where
  wsGetMessage :: WebSocket -> IO a
  wsPutMessage :: WebSocket -> a -> IO ()

instance WebSocketMessage ByteString where
 -- wsGetMessage :: WebSocket -> IO B.ByteString
  wsGetMessage cgir = do
    Frame _ _ _ bs <- readChan (wsRecv cgir)
    return bs

instance WebSocketMessage String where
 -- wsGetMessage :: WebSocket -> IO ByteString
  wsGetMessage cgir = do
    Frame _ _ _ bs <- readChan (wsRecv cgir)
    return $ asByteString bs

getWS :: Socket -> ByteString -> IO WebSocket
getWS sock hdx = do
    env <- newEmptyMVar 
    chan <- newChan
    _ <- forkIO $ readHdrs env chan zilde
    wchan <- newChan :: IO (Chan Message)
    sendAll sock hdx
    _ <- forkIO $ forever $ writeMsg wchan sock
    return $ WebSocket wchan env chan
  where writeMsg x k = do
              buf <- readChan x
              case buf of
                Close -> sClose k
                Binary bs -> 
                  let ef = encodeFrame zilde BinaryFrame bs
                   in sendAll k ef -- (BC.pack $ map (chr . fromIntegral) (B.unpack bs))
                Text str ->
                  let ef = encodeFrame zilde TextFrame ( asByteString str)
                   in sendAll k ef

        breakSubstrings b s = let (f,l) = B.breakSubstring b s
                               in if strNull l then [f] else f : breakSubstrings b (strDrop (strLen b) l)
        readHdrs env ch lft = do
              more <- sktRecv sock 4096 :: IO ByteString
              let utn = strCat [lft, more]
                  (prev,post) = B.breakSubstring "\r\n\r\n" utn
              if strNull post then readHdrs env ch utn
              else do let lns = breakSubstrings "\r\n" prev
                          hds = map split (tail lns)
                      putMVar env hds
                      readMsgs ch (strDrop 4 post)
        readMsgs ch lft = do
            let (frm, lfto) = decodeFrame lft
            print ("readMsg "::String,frm,lfto)
            case frm of 
              Just frmx -> writeChan ch frmx
              Nothing -> return ()
            more <- sktRecv sock 4096 :: IO ByteString
            readMsgs ch (strCat [lfto, more])
        split :: ByteString -> (String, String)
        split str = let (key, val) = strBrk (== ':') str
                        (ign, valx) = strBrk (/= ' ') (strDrop 1 val)
                     in ( toStr key, toStr valx)


doHandshake k fl hds = do
    let wsk = case (lookup "Sec-WebSocket-Key" hds) of {Nothing -> zilde; Just b -> asByteString b}
        hash = hashKey wsk
        encoded = B64.encode hash
    print encoded
    let rsp = "HTTP/1.1 101 Web Socket Protocol Handshake\r\nUpgrade: WebSocket\r\nConnection: Upgrade\r\n"
        rsp2 = "Sec-WebSocket-Accept: "++(toStr encoded)++"\r\n"
        rsp3 = "Server: hs-www/WSServer\r\n\r\n"
    sktSend k (asByteString (rsp ++ rsp2 ++ rsp3))
  
getServerWS :: Socket -> IO WebSocket
getServerWS sock = do
    env <- newEmptyMVar 
    chan <- newChan
    _ <- forkIO $ readHdrs env chan zilde
    wchan <- newChan :: IO (Chan Message)
    _ <- forkIO $ forever $ writeMsg wchan sock
    return $ WebSocket wchan env chan
  where writeMsg x k = do
              buf <- readChan x
              case buf of
                Close -> sClose k
                Binary bs -> 
                  let ef = encodeFrame zilde BinaryFrame bs
                   in sendAll k ef -- (BC.pack $ map (chr . fromIntegral) (B.unpack bs))
                Text str ->
                  let ef = encodeFrame zilde TextFrame ( asByteString str)
                   in sendAll k ef

        breakSubstrings b s = let (f,l) = B.breakSubstring b s
                               in if strNull l then [f] else f : breakSubstrings b (strDrop (strLen b) l)
        readHdrs env ch lft = do
              print ("lft ",lft)
              more <- sktRecv sock 4096 :: IO ByteString
              print ("more ", more)
              let utn = strCat [lft, more]
                  (prev,post) = B.breakSubstring "\r\n\r\n" utn
              if strNull post then readHdrs env ch utn
              else do let lns = breakSubstrings "\r\n" prev
                          hds = map split (tail lns)
                          fl = head lns
                      print ("putting ", toStr fl, hds)
                      putMVar env (("First Line", toStr fl) : hds)
                      
                      doHandshake sock fl hds
                      
                      readMsgs ch (strDrop 4 post)
        readMsgs ch lft = do
            let (frm, lfto) = decodeFrame lft
            case frm of 
              Just frmx -> writeChan ch frmx
              Nothing -> return ()
            more <- sktRecv sock 4096 :: IO ByteString
            if strLen more <= 0 then return () else readMsgs ch (strCat [lfto, more])
        split :: ByteString -> (String, String)
        split str = let (key, val) = strBrk (== ':') str
                        (ign, valx) = strBrk (/= ' ') (strDrop 1 val)
                     in ( toStr key, toStr valx)

toStr :: ByteString -> String
toStr = byteStringToString


sendAll :: Socket -> ByteString -> IO ()
sendAll s bs = do 
  sent <- sktSend s bs
  let res = strDrop (fromIntegral sent) bs
  if strNull res then return () else sendAll s res 

sendMessage :: WebSocket -> Message -> IO ()
sendMessage rsp x = writeChan (wsSend rsp) x

-- | runSCGI threads port cgi-function (in the IO monad)
runServer :: WebSocketMessage a => Int -> Int -> (a -> IO () ) -> (WebSocket -> IO() )-> IO ()
runServer maxThreads port f g = join (doListen port <$> handler f g <$> newQSem maxThreads )

doListen :: Int -> (Socket -> IO ()) -> IO ()
doListen port loop = withSocketsDo $ bracket (listenOn (PortNumber (fromIntegral port))) sClose loop

handler :: WebSocketMessage a => (a -> IO ()) -> (WebSocket -> IO ()) -> QSem -> Socket -> IO ()
handler f g qsem socket = do
  waitQSem qsem
  (sock, _) <- sktAccept socket
  _ <- forkIO $ do
      catch (doWebSocket f g sock) (\e -> hPutStrLn stderr $ "websocket: "++show (e::SomeException))
      signalQSem qsem
  handler f g qsem socket

doWebSocket :: WebSocketMessage a => (a -> IO() ) -> (WebSocket -> IO ()) -> Socket -> IO ()
doWebSocket f g sock = do
  a <- getServerWS sock
  -- what I do here is
  -- a) call the application for initialization (with the connection parms)
  -- b) call the application for each msg received ?
  -- OR
  -- the expectation is that the application loops forever and pulls messages from the Channel
  forkIO $ forever $  wsGetMessage a >>= f -- call the fn when I get a message?
  g a
