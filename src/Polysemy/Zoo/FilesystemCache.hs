{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Zoo.FilesystemCache where



import           Codec.Serialise as S
import           Crypto.Hash.MD5 as MD5
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Serialize as Z
import           Data.Serialize.Text ()
import           Data.Vector.Serialize ()
import           Polysemy.Time
import           Polysemy.Zoo.Utils
import           System.FilePattern.Directory


data FilesystemCache m a where
  DropFsCached  :: Foldable f => f FsCacheBucket  -> FilesystemCache m ()
  FsCached      :: FsCacheKey     -> (a -> ByteString) -> (ByteString -> m a) -> m a -> FilesystemCache m a

data FsCacheKey = FsCacheKey        { cName         :: FsCacheBucket
                                    , cHash         :: ByteString }

type CacheTTL = Map FsCacheBucket Seconds

type FsCacheBucket = Text
type FsCacheBuckets = Set FsCacheBucket

makeSem ''FilesystemCache

makeLensesWith (lensField .~ mappingNamer (\f -> [f <> "_"]) $ lensRules) ''FsCacheKey

initFsCacheKey :: FsCacheKeyable a => FsCacheBucket -> a -> FsCacheKey
initFsCacheKey n = appendFsCacheKey $ initializeBucket n

initializeBucket :: FsCacheBucket -> FsCacheKey
initializeBucket n = FsCacheKey n "initialized"

appendFsCacheKey :: FsCacheKeyable a => FsCacheKey -> a -> FsCacheKey
appendFsCacheKey key add = key & cHash_ %~ \h -> toCacheHash h add

instance Show FsCacheKey where 
  show key = concat [show $ cName key, "_", toS $ fromUtf8 $ B16.encode $ cHash key]

class FsCacheKeyable a where
  cacheInputToBytestring :: a -> [ByteString]

instance {-# OVERLAPS #-} FsCacheKeyable [String] where
  cacheInputToBytestring = fmap toUtf8

instance ConvertText a Text => FsCacheKeyable a where
  cacheInputToBytestring = pure . toUtf8

instance {-# OVERLAPS #-} (FsCacheKeyable a, FsCacheKeyable b) => FsCacheKeyable (a,b) where
  cacheInputToBytestring (a,b) = cacheInputToBytestring a <> cacheInputToBytestring b

toCacheHash :: FsCacheKeyable a => ByteString -> a -> ByteString
toCacheHash init = foldl' (\h -> MD5.hash . (h <>)) init . cacheInputToBytestring

dontCache :: FsCacheKey
dontCache = initializeBucket "Uncached"


runFilesystemCache :: Members '[Log, ErrorE, GhcTime, Embed IO] r => FilePath -> CacheTTL -> FsCacheBuckets
  -> Sem (FilesystemCache ': r) a -> Sem r a
runFilesystemCache folder cacheTtl cacheBuckets = interpretH $ \case
  FsCached key enc dec action -> 
    if cacheEnabled || isJust ttl then
      bool (done recalc "FilesystemCache miss") hit =<< embedCatch (doesFileExist path)
      else runTSimple action
    where ttl = cacheTtl ^? ix (cName key)
          cacheEnabled = elem (cName key) $ cacheBuckets
          path = folder </> show key
          hit = do age <- since =<< embedCatch (getModificationTime path)
                   log Trace $ shot (ttl, age)
                   if Just True == fmap (age >=) ttl then done recalc "FilesystemCache expired"
                     else done read "FilesystemCache hit"
          done ac msg = log Debug (msg <> " " <> toS path) >> ac
          read = (\bs -> runTSimple $ dec bs) =<< embedCatch (B.readFile path)
          recalc = do r <- runTSimple action
                      ins <- getInspectorT
                      slap r $ forM (inspect ins r) $ \rSuccess ->
                        embedCatch $ do createDirectoryIfMissing True (takeDirectory path)
                                        B.writeFile path (enc rSuccess)
  DropFsCached names    -> pureT =<< mapM_ g names
    where g k   = do let globPattern = show k <> "_*"
                     files <- embedCatch $ getDirectoryFiles folder [globPattern]
                     forM_ files $ \p -> do  log Info ("FilesystemCache: droppeding " <> toS p <> "...")
                                             embedCatch (removeFile $ folder </> p)
                     when (null files) $ log Info $ toS $ "No cache files found in " <> folder <> " matching " <> show globPattern


fsCachedSerialized :: (Serialize a, Members '[FilesystemCache] r) => FsCacheKey -> Sem r a -> Sem r a
fsCachedSerialized ck ac = fsCached ck Z.encode (eitherErrorM . Z.decode) ac

fsCachedLByteString :: (Members '[FilesystemCache] r) => FsCacheKey -> Sem r LByteString -> Sem r LByteString
fsCachedLByteString ck = fsCached ck toS $ pure . toS

fsCachedByteString :: (Members '[FilesystemCache] r) => FsCacheKey -> Sem r ByteString -> Sem r ByteString
fsCachedByteString ck = fsCached ck id pure

fsCachedSerialised :: (Serialise a, Members '[FilesystemCache] r) => FsCacheKey -> Sem r a -> Sem r a
fsCachedSerialised ck ac = fsCached ck (toS . S.serialise) (eitherErrorShowM . S.deserialiseOrFail . toS) ac
