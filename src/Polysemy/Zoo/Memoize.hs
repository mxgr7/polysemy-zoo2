{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Zoo.Memoize where

import qualified Data.Cache as C
import           Data.Serialize.Text ()
import           Data.Vector.Serialize ()
import           Foreign.Store
import           GHC.Stack (SrcLoc(..))
import           Polysemy.AtomicState
import qualified Polysemy.Cache as PC
import           Polysemy.Log
import           Polysemy.Zoo.Utils hiding (SrcLoc)
import qualified Prelude as P

type MemoizeCache            = C.Cache (TypeRep, MemoizeKey, MinimalSrcLoc) Any
type MemoizeCacheEffect      = PC.Cache (TypeRep, MemoizeKey, MinimalSrcLoc) Any
type MemoizeCacheStateEffect      = AtomicState MemoizeCache
type MemoizeKey                 = [String]
type MinimalSrcLoc              = (FilePath, Int, Int)

data Memoize m a where
  UnsafeMemoize     :: (Typeable a) => Bool -- ^ write Cache hits to log
                    -> SrcLoc -> MemoizeKey -> m a -> Memoize m a

makeSem ''Memoize

-- * Interpreters

runMemoizeInCacheEffect :: forall a r . (HasCallStack, Members '[MemoizeCacheEffect, Log, GhcTime] r)
  => Sem (Memoize : r) a -> Sem r a
runMemoizeInCacheEffect = interpretH toTactical
  where
    toTactical :: forall b (rInitial :: EffectRow) . Memoize (Sem rInitial) b -> Tactical Memoize (Sem rInitial) r b
    toTactical (UnsafeMemoize doLog loc key action) = do
        PC.lookup fullKey >>= maybe miss hit
      where fullKey = (tr, key, (srcLocFile loc, srcLocStartLine loc, srcLocStartCol loc))
            tr = typeRep $ Proxy @b
            showKey = mconcat [": ", shot key, ", Type ", shot tr]
            hit entry = when doLog (log Debug $ "Memoize cache hit" <> showKey) >> pureT (unsafeCoerce entry)
            miss = measureLog Debug (const $ "Memoize cache miss" <> showKey) $ do
              r <- runTSimple action
              ins <- getInspectorT
              slap r $ forM (inspect ins r) $ \rSuccess -> seq rSuccess $ PC.insert fullKey (unsafeCoerce rSuccess)

runMemoizeCacheEffect :: (Member (Final IO) r, Member (Embed IO) r) =>
      Maybe (IORef MemoizeCache) -> Sem (MemoizeCacheEffect : MemoizeCacheStateEffect : r) b -> Sem r b
runMemoizeCacheEffect cacheRefM = bind (maybe (embedFinal newMemoizeCache) pure cacheRefM)
                               . flip runAtomicStateIORef . PC.runCacheAtomicState

runMemoize :: Members '[Final IO, Embed IO, Log, GhcTime] r => Maybe (IORef MemoizeCache) -> 
  Sem (Memoize : MemoizeCacheEffect : MemoizeCacheStateEffect : r) a -> Sem r a
runMemoize cacheRefM = runMemoizeCacheEffect cacheRefM . runMemoizeInCacheEffect

runMemoizeInIO :: Sem '[Memoize, MemoizeCacheEffect, MemoizeCacheStateEffect, Log
                       , Error Text, GhcTime, Embed IO, Final IO] a -> IO a
runMemoizeInIO = runFinal . embedToFinal . interpretTimeGhc . errorToIOFinalThrow . interpretLogStderr . runMemoize Nothing

runMemoizedForeignStore :: Members '[Final IO, Embed IO, Log, GhcTime] r
                        => Sem (Memoize : MemoizeCacheEffect : MemoizeCacheStateEffect : r) a -> Sem r a
runMemoizedForeignStore ac = do ref <- embedFinal recoverMemoizeCacheFromStore
                                runMemoize (Just ref) ac

-- * Actions

runMemoized :: (TE r, HasCallStack, Typeable a, Members '[Memoize] r) => MemoizeKey -> Sem r a -> Sem r a
runMemoized k a = withCallStackLoc $ \loc -> unsafeMemoize False loc k a

runMemoizedLog :: (TE r, HasCallStack, Typeable a, Members '[Memoize] r) => MemoizeKey -> Sem r a -> Sem r a
runMemoizedLog k a = withCallStackLoc $ \loc -> unsafeMemoize True loc k a

-- * Helpers 

withCallStackLoc :: (HasCallStack, TE r) => (SrcLoc -> Sem r a) -> Sem r a
withCallStackLoc = forMaybe (throw "Did you add the 'HasCallStack' constraint") $
  fmap snd $ headMaybe . snd =<< uncons (getCallStack callStack)

newMemoizeCache :: IO (IORef (C.Cache k v))
newMemoizeCache = newIORef =<< C.newCache Nothing

memoizeForeignStoreIndex :: Word32
memoizeForeignStoreIndex = 3333333333

recoverMemoizeCacheFromStore :: forall k v . (Typeable k, Typeable v) => IO (IORef (C.Cache k v))
recoverMemoizeCacheFromStore = lookupStore memoizeForeignStoreIndex >>= \case
  Just st -> ffor (readStore st) $ \d -> fromDyn d $ P.error $ "type mismatch, expecting "
                                         <> show (typeRep $ Proxy @(C.Cache k v)) <> " got " <> show (dynTypeRep d)
  Nothing -> newMemoizeCache >>= \r -> slap r (writeStore (Store memoizeForeignStoreIndex) $ toDyn r)

