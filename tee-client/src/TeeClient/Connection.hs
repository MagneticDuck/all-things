module TeeClient.Connection
  -- exports -- {{{
  ( 
  -- * Speaking the Language (ByteStrings and Word8s)
  -- ** Constructors
  B.ByteString 
  , Word8
  , packStr , packWords 
  , charToWord
  -- ** Accessors
  , unpackStr, unpackWords
  , wordToChar
  -- ** Combinators
  , bsAppend
  -- * Taking Aim and Throwing (RemoteAddr) 
  -- ** Constructors
  , RemoteAddr(..)
  -- *** Some Requests
  , listRequest
  , infoRequest
  -- ** Accessors
  -- *** Sending 
  , sendPacket, sendPacketTo
  , makeRequest, makeRequestTo
  , makeRepeatRequest, makeRepeatRequestTo
  , makeRequests
  -- * Misc. Utilities
  , hostResolves
  , getMasterSrv
  , makeRequestToMaster
  , mapConcDelay
  , timeOperation
  , hexify
  ) where -- }}}

-- imports  -- {{{
import Control.Monad
import Network.Socket hiding (sendTo, recv)
import Network.Socket.ByteString (sendTo, recv)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C 
import Data.Word8 (Word8)
import Data.Maybe
import System.CPUTime
import Control.Applicative
import Control.Exception 
import System.Timeout 
import Control.Concurrent
import Control.DeepSeq
import Control.Concurrent.Async -- }}}

-- ByteStrings and Word8s
-- packStr, charToWord, [un]packWords  -- {{{
packStr :: String -> B.ByteString
packStr = C.pack

unpackStr :: B.ByteString -> String
unpackStr = C.unpack

charToWord :: Char -> Word8
charToWord = toEnum . fromEnum

wordToChar :: Word8 -> Char
wordToChar = toEnum . fromEnum

packWords :: [Word8] -> B.ByteString
packWords = B.pack

unpackWords :: B.ByteString -> [Word8]
unpackWords = B.unpack

bsAppend :: B.ByteString -> B.ByteString -> B.ByteString
bsAppend = B.append
 -- }}}
 
-- Aiming Packets
-- RemoteAddr, UdpHandle, {open,close}UdpHandle, doUdpHandle -- {{{
-- | the domain name / IP and port to aim packets at
data RemoteAddr = 
  RemoteAddr 
    { targetPort :: Int
    , targetName :: String } deriving (Show, Eq, Read) 

data UdpHandle = 
  UdpHandle 
    { uSock :: Socket
    , uAddr :: SockAddr}

openUdpHandle :: RemoteAddr -> IO UdpHandle   
openUdpHandle (RemoteAddr port hostname) =
  do 
--     putStr "O"
    addrInfo <- getAddrInfo Nothing (Just hostname) (Just $ show port)
    let serveraddr = head addrInfo
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    return $ UdpHandle sock (addrAddress serveraddr)

closeUdpHandle :: UdpHandle -> IO ()
closeUdpHandle = 
--   (>> putStr "C") 
    sClose . uSock

doUdpHandle :: RemoteAddr -> (UdpHandle -> IO a) -> IO a
doUdpHandle target f =
  do
    h <- openUdpHandle target
    f h <* closeUdpHandle h
 -- }}}
 
-- Throwing Packets
-- sendPacketToHandle, makePacket[To], makeRequest[To] -- {{{
sendPacketToHandle :: UdpHandle -> B.ByteString -> IO ()
sendPacketToHandle h str = void $ sendTo (uSock h) str (uAddr h)

-- | throw some data at a host and run away
sendPacket :: B.ByteString -> RemoteAddr -> IO ()
sendPacket = flip sendPacketTo

-- | throw at a host some data and run away
sendPacketTo :: RemoteAddr -> B.ByteString -> IO ()
sendPacketTo target str =
  doUdpHandle target (`sendPacketToHandle` str) 

packetTimeout :: Int
packetTimeout = 5 * (10^(5::Int))

checkSocket :: Socket -> IO (Maybe B.ByteString)
checkSocket = timeout packetTimeout . flip recv 4096

accSocket :: Socket -> IO (Maybe [B.ByteString])
accSocket sock =
  do
    check <- checkSocket sock
    case check of 
      Just a -> do
        next <- accSocket sock
        case next of
          Just b -> return . Just $ a : b
          Nothing -> return $ Just [a]
      Nothing -> return Nothing

-- | throw some data at a host and write down his response
--
-- (this is UDP, not TCP, so it just records all the packet data sent until nothing happens for 500 ms)
makeRequest :: B.ByteString -> RemoteAddr -> IO [B.ByteString]
makeRequest request host =
  doUdpHandle host $
    \h -> do
      sendPacketToHandle h request
      concat . maybeToList <$> accSocket (uSock h)

-- | throw at a host some data and write down his response
makeRequestTo :: RemoteAddr -> B.ByteString -> IO [B.ByteString]
makeRequestTo = flip makeRequest

 -- }}}

-- makeRepeatRequest[To], makeRequests -- {{{
-- | throw at some data at a host and write down his response
--
-- similar to makeRequest, but tries repeatedly if no data is returned
makeRepeatRequest :: Int -> B.ByteString -> RemoteAddr -> IO [B.ByteString]
makeRepeatRequest 0 _ _  = return []
makeRepeatRequest tries request host =
  do
    response <- makeRequest request host
    case response of
      [] -> makeRepeatRequest (tries - 1) request host
      _ -> return response

-- | throw at a host some data and write down his response
makeRepeatRequestTo :: Int -> RemoteAddr -> B.ByteString -> IO [B.ByteString]
makeRepeatRequestTo = fmap flip makeRepeatRequest

-- | make a bunch of requests! uses concurrent magic to make things not take forever (100 ms between each request)
makeRequests :: [(B.ByteString, RemoteAddr)] -> IO [[B.ByteString]]
makeRequests = mapConcDelay 10000 $ uncurry makeRequest  -- }}}

-- Writing Requests
-- requestFromCode, listRequest 
requestFromCode :: String -> B.ByteString
requestFromCode = B.append (B.pack $ replicate 10 255) . C.pack

-- | a request that will make a master server throw a bunch of IPs at you
listRequest :: B.ByteString
listRequest = requestFromCode "req2"

-- | a request that will make a game server spit a bunch of info at you
infoRequest :: B.ByteString
infoRequest = requestFromCode ("gie3" ++ [toEnum 4])
 
-- Various Utilities
-- hostResolves, getMasterSrv, makeRequestToMaster, mapConcDelay -- {{{
-- | check if a host resolves
hostResolves :: HostName -> IO Bool
hostResolves hostname =
  handle (const (return False) . (id :: SomeException -> SomeException)) $ 
    (not . null) <$> getAddrInfo Nothing (Just hostname) Nothing

-- | find the teeworlds master servers
getMasterSrv :: IO [String]
getMasterSrv =
  fmap (map fst . filter snd) . mapM (\i -> ((,) $ f i) <$> hostResolves (f i)) $ [(1 :: Int)..4]
  where f i = "master" ++ show i ++ ".teeworlds.com"

-- | throw some data at the teeworlds master servers and wait for a reply
makeRequestToMaster :: B.ByteString -> IO [[B.ByteString]]
makeRequestToMaster request =
  mapM (makeRequest request) =<< map (RemoteAddr 8300) <$> getMasterSrv

-- | like mapConcurrently, but spaces the actions out by a certain number of microseconds
mapConcDelay :: Int -> (a -> IO b) -> [a] -> IO [b]
mapConcDelay ms f xs = 
  mapConcurrently id $ zipWith (>>) (map threadDelay [0, ms..]) (map f xs)

-- | time (in seconds) it takes to complete an action
timeOperation :: (NFData a) => IO a -> IO (a, Float)
timeOperation x =
  do
    start <- getCPUTime 
    v <- x
    end <- getCPUTime
    return (v, fromIntegral (end - start) / (10^(12::Int)))
 -- }}}
 
-- Hexifying
-- wordToHex, hexify 
wordToHex :: (Integral a) => a -> String
wordToHex x =
  f (x `div` 16) : [f $ x `mod` 16]
  where f = ("0123456789abcdef" !!) . fromIntegral 

-- | make a beautiful hexified bytestring
hexify :: B.ByteString -> String
hexify = unwords . map wordToHex . B.unpack
 
