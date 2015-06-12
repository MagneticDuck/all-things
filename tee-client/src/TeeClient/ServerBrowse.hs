module TeeClient.ServerBrowse
  -- exports {{{
  ( -- * TargetList
    TargetList
  , getTargetList 
  -- * ServInfo
  , ServInfo(..)
  , servVersion, servName, servMap, servMod, servPlayers
  , extractServInfo
  , getServInfo
  -- * GameServer
  , GameServer
  , getGameServers
  , cacheGameServers, readGameServers, searchGameServers)  where -- }}}

-- imports -- {{{
import TeeClient.Connection
import Control.Applicative
import Data.List
import Data.List.Split
import Data.Either 
import Control.Concurrent.Async 
import Lens.Family2
import Lens.Family2.TH
import Data.Maybe -- }}}

-- TargetList
-- getMasterTargets 
getMasterTargets :: IO [RemoteAddr]
getMasterTargets =
  map (RemoteAddr 8300) <$> getMasterSrv 

-- getTargetList -- {{{
{- |
  list of all the [RemoteAddr]s of all the game servers
  usually a pretty large list, around 1000 elements
-} 
type TargetList = [RemoteAddr]

breakBy :: Int -> [a] -> [[a]]
breakBy i xs =
  if length xs <= i then [xs]
  else take i xs : breakBy i (drop i xs)

packetToAddrData :: [Word8] -> [[Word8]]
packetToAddrData = 
  map (drop 2 . reverse . dropWhile (== 0) . reverse) . breakBy 18 . drop 24

addrDataToTarget :: [Word8] -> Either RemoteAddr [Word8]
addrDataToTarget xs@[_, _, _, _, _, _] =
  Left $ RemoteAddr (e*256 + f) (intercalate "." . map show $ [a, b, c, d])
  where [a, b, c, d, e, f] = map (fromIntegral :: Word8 -> Int) xs
addrDataToTarget xs@_ = Right xs

extractTargetsFromPacket :: [Word8] -> [Either RemoteAddr [Word8]]
extractTargetsFromPacket = map addrDataToTarget . packetToAddrData  
  
requestTargetList :: RemoteAddr -> IO [Either RemoteAddr [Word8]]
requestTargetList target =
  nub . concatMap (extractTargetsFromPacket . unpackWords) <$> makeRequest listRequest target 

-- | gets the list of all the game servers
getTargetList :: IO [RemoteAddr]
getTargetList = fmap (nub . lefts . concat) . mapConcurrently requestTargetList =<< getMasterTargets 
 -- }}}

-- ServInfo
-- getServInfo -- {{{
-- | information about a certain server
data ServInfo =
  ServInfo
    { _servVersion :: String
    , _servName :: String 
    , _servMap :: String
    , _servMod :: String
    , _servPlayers :: [String]} deriving (Show, Read, Eq)

servInfo :: ServInfo
servInfo = ServInfo [] [] [] [] []

$(makeLenses ''ServInfo)
servVersion, servName, servMap, servMod :: (Functor f) => LensLike' f ServInfo String
servPlayers :: (Functor f) => LensLike' f ServInfo [String]

extractServInfo :: [Word8] -> [[Word8]]
extractServInfo = drop 1 . splitOn [0] . drop 15

maybeIndex :: Int -> [a] -> Maybe a
maybeIndex i xs =
  if i > length xs then Nothing
    else Just $ xs !! (i - 1)

parseServInfo :: [[Word8]] -> ServInfo
parseServInfo xs = 
  ($ servInfo) . foldl (.) id . catMaybes $
    [ setterIsAt (servVersion .~) 1 
    , setterIsAt (servName .~) 2
    , setterIsAt (servMap .~) 3
    , setterIsAt (servMod .~) 4 ]
  where
    setterIsAt s i = (s <$>) . fmap (unpackStr . packWords) $ maybeIndex i xs
    
-- | ask a server about itself
getServInfo :: RemoteAddr -> IO (Maybe ServInfo)
getServInfo target = 
  do
    r <- makeRepeatRequest 3 infoRequest target
    case r of
      (x:_) -> return . Just . parseServInfo . extractServInfo . unpackWords $ x
      _ -> return Nothing
 -- }}}

-- GameServer
-- | a target and some info about it
type GameServer = (RemoteAddr, ServInfo) 

-- | find and interrogate all the game servers (this takes around 8 seconds)
getGameServers :: IO [GameServer]
getGameServers = 
  fmap 
    (mapMaybe $ uncurry (liftA (<$>) (,)))
    (getTargetList >>= mapConcDelay 5000 (\x -> (,) x <$> getServInfo x))

cacheGameServers :: IO ()
cacheGameServers = writeFile "/tmp/teeclient-cacheGameServers" =<< show <$> getGameServers

readGameServers :: IO [GameServer]
readGameServers = read <$> readFile "/tmp/teeclient-cacheGameServers"

searchGameServers :: String -> IO [GameServer]
searchGameServers str =
  filter (isInfixOf str . _servName . snd) <$> readGameServers
