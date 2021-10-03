
module Polysemy.Zoo.Utils
  (module Polysemy.Zoo.Utils
  ,module Reexport
  )where

import           Codec.Archive.Zip as Z
import qualified Control.Lens as Lens
import qualified Data.Attoparsec.Text as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Map as M
import qualified Data.Text as T
import           Formatting.Formatters
import qualified Hoff as H
import           Polysemy.Zoo.Prelude as Reexport
import qualified Prelude as P
import           System.IO.Error

-- * error handling + writing

newtype TextError = TextError Text
                  deriving (Generic)

instance Exception TextError

instance Show TextError where
  show (TextError t) = toS t

type ErrorMsg = Text
type ErrorMsgs = [Text]

type ErrorE = Error ErrorMsg
type WriterE = Writer ErrorMsgs

type TE r = Member ErrorE r
type TW r = Member WriterE r

embedCatch :: Members '[Embed IO, ErrorE] r => IO a -> Sem r a
embedCatch = fromExceptionVia $ \e -> shot @SomeException e
{-# INLINE embedCatch #-}


-- | because it catches WrappedExc thrown by errorToIOFinal
semCatchNotUseful :: Members '[Final IO, ErrorE] r => Sem r a -> Sem r a
semCatchNotUseful = fromExceptionSemVia $ \e -> shot @SomeException e


tellErrors :: (Foldable f, TE r, TW r) => f (Sem r a) -> Sem r [a]
tellErrors = mapTellErrors id
{-# INLINE tellErrors #-}

mapTellErrors :: (Foldable f, TW r, TE r) => (v -> Sem r a) -> f v -> Sem r [a]
mapTellErrors f xs = do
  (errs, vals) <- fmap partitionEithers $ mapM (try . f) $ toList xs
  vals <$ tell errs
{-# INLINABLE mapTellErrors #-}

collectErrors :: (Foldable f, TE r) => (v -> Sem r a) -> f v -> Sem r [a]
collectErrors f xs = do
  (errs, vals) <- fmap partitionEithers $ mapM (try . f) $ toList xs
  vals <$ when (not $ null errs) (throw $ T.unlines errs)
{-# INLINABLE collectErrors #-}


collectErrors2 :: TE r => Sem r a -> Sem r b -> Sem r (a,b)
collectErrors2 a b = do
  aE <- try a
  bE <- try b
  case (aE, bE) of
    (Left ae, Left be) -> throw $ unlines [ae,be]
    _ -> fromEither $ (,) <$> aE <*> bE
{-# INLINABLE collectErrors2 #-}

mapTellErrors' :: (Foldable f, Member (Writer [a]) r) => (v -> Either a b) -> f v -> Sem r [b]
mapTellErrors' f xs = let (errs, vals) = partitionEithers $ fmap f $ toList xs in vals <$ tell errs
{-# INLINABLE mapTellErrors' #-}

logWriterMsgs :: (HasCallStack, Member Log r) => Severity -> Sem (Writer ErrorMsgs ': r) a -> Sem r a
logWriterMsgs sev a = do
  (errs, r) <- runWriterAssocR a
  r <$ mapM (log sev) errs
{-# INLINABLE logWriterMsgs #-}

throwWriterMsgs :: TE r => Sem (Writer ErrorMsgs ': r) a -> Sem r a
throwWriterMsgs a = do
  (errs, r) <- runWriterAssocR a
  if null errs then pure r else throw $ unlines errs
{-# INLINABLE throwWriterMsgs #-}

fromEitherWith :: Member (Error g) r => (e -> g) -> Either e a -> Sem r a
fromEitherWith f = either (throw . f) pure
{-# INLINABLE fromEitherWith #-}

errorToIOFinalThrow :: Members '[Final IO] r => Sem (Error Text : r) a -> Sem r a
errorToIOFinalThrow s = embedFinal . either (throwIO . TextError) pure =<< errorToIOFinal s
{-# INLINABLE errorToIOFinalThrow #-}

prependMsg :: TW r => ErrorMsg -> Sem r a -> Sem r a
prependMsg msg = censor $ fmap (msg <>)
{-# INLINABLE prependMsg #-}

-- * duplicates

groupByHm :: (Eq k, Hashable k, Semigroup (f a), Applicative f, Foldable t) => t (k,a) -> HM.HashMap k (f a)
groupByHm = HM.fromListWith (<>) . fmap (second pure) . toList
{-# INLINABLE groupByHm #-}

fromListByTellDups :: forall a t v r. (TW r, Typeable a, Eq a, Hashable a, Show a, Foldable t)
  => t (a,v) -> Sem r (HM.HashMap a v)
fromListByTellDups xs = HM.traverseWithKey g $ (groupByHm xs :: HM.HashMap a (NonEmpty v))
  where g _ (x:|[])     = pure x
        g k (x:|r)      = x <$ tell ["Key (" <> showt (typeOf k) <> ") " <> shot k <> " appears " <> showt (succ $ length r) <> " times" :: Text]
        g :: a -> NonEmpty v -> Sem r v
{-# INLINABLE fromListByTellDups #-}

extract :: Functor f => (t -> b) -> f t -> f (b, t)
extract f = fmap (\x -> (f x, x))
{-# INLINABLE extract #-}

fromListByTellDups' :: forall a t v r. (TW r, Functor t, Typeable a, Eq a, Hashable a, Show a, Foldable t)
  => (v -> a) -> t v -> Sem r (HM.HashMap a v)
fromListByTellDups' f = fromListByTellDups . extract f
{-# INLINABLE fromListByTellDups' #-}

-- fromListNoDupsBy :: forall t r v a . (TE r, Typeable a, Eq a, Hashable a, Show a, Foldable t) => (v -> a) -> t v -> Sem r (HM.HashMap a v)
-- fromListNoDupsBy f xs | length res == length xs = pure res
--                       | True                    = res <$ mapErrors g (HM.toList $ groupByHm f xs)
--   where g (_, (_:|[])) = pure ()
--         g (k, x) = throwError $ "Duplicate " <> showt (typeOf k) <> " (" <> showt (length x) <> " occurences)"
--         res = HM.fromList $ fmap (\x -> (f x, x)) $ toList xs

-- todo: check if already part of env variable
-- prependEnv :: String -> String -> IO ()
-- prependEnv name value = setEnv name . maybe value ((value <> ";") <>) =<< lookupEnv name



-- * misc

patternToPath :: Day -> FilePath -> FilePath
patternToPath d p = formatTime defaultTimeLocale p (toBaseDay d)
{-# INLINABLE patternToPath #-}

getSingleFileZip :: FilePath -- ^ zip file path
                 -> IO LByteString
getSingleFileZip path = do ar <- Z.toArchive <$> BL.readFile path
                           case Z.filesInArchive ar of
                             [entry]    -> getFileFromZip' path entry ar
                             x          -> ioError $ userError $ "Expected zip archive with single entry, got:\n" <> show x <>
                                           "\nin file: " <> path
{-# INLINABLE getSingleFileZip #-}

getFileFromZip' :: FilePath -- ^ zip file path
                -> FilePath -- ^ entry path
                -> Z.Archive
                -> IO LByteString
getFileFromZip' path entry archive = 
  maybeThrow (userError $ "Entry no found in archive " <> path) $ fmap Z.fromEntry $ Z.findEntryByPath entry archive
{-# INLINABLE getFileFromZip' #-}

getFileFromZip :: FilePath -- ^ zip file path
               -> FilePath -- ^ entry path
               -> IO LByteString
getFileFromZip path entry = BL.readFile path >>= getFileFromZip' path entry . Z.toArchive
{-# INLINABLE getFileFromZip #-}

-- * measure elapsed time

showDuration :: (ConvertText LText b, TimeUnit u) => u -> b
showDuration u = toS $ format (fixed 5 % "s") $ secondsFrac $ toNanos u
{-# INLINABLE showDuration #-}

measureLog' :: (HasCallStack, Members '[Log, GhcTime] r) => Severity -> Sem r a -> Sem r a
measureLog' sev sem = withFrozenCallStack $ measureLog sev (const "") sem

measureLog :: (HasCallStack, Members '[Log, GhcTime] r) => Severity -> (a -> Text) -> Sem r a -> Sem r a
measureLog sev msg ac = withFrozenCallStack $ do (elapsed, r) <- measure ac
                                                 slap r $ log sev $ "(" <> showDuration elapsed <> ") " <> msg r


-- * error  handling using Prelude.error (rethink this!)
eitherErrorWithM :: (Applicative m,  ConvertText s String) => (e -> s) -> Either e a -> m a
eitherErrorWithM f = either (P.error . toS . f) pure
{-# INLINABLE eitherErrorWithM #-}

eitherErrorShowM :: (Applicative m,  Show s) => Either s a -> m a
eitherErrorShowM = eitherErrorWithM show
{-# INLINABLE eitherErrorShowM #-}

eitherErrorM :: (Applicative m,  ConvertText s String) => Either s a -> m a
eitherErrorM = eitherErrorWithM id
{-# INLINABLE eitherErrorM #-}

-- * lookup
intersectWriteMissing :: (Ord k, Foldable f, TW r) => (k->Text) -> (b -> k) -> (a -> b -> c) -> Map k a -> f b -> Sem r (Map k c)
intersectWriteMissing toMsg getKey combine map keys' = found <$ tell (toMsg <$> M.keys (M.difference keys found))
  where found = M.intersectionWith combine map keys
        keys = M.fromList $ extract getKey $ toList keys'
        
instance IsString Day where
  fromString  = either P.error dateToDay . A.parseOnly (parser_Ymd_lenient <* A.endOfInput) . toS

lookupError'
  :: (TE r, Show b, FoldableWithIndex b t, Lens.Ixed (t a), Typeable b, Lens.Index (t a) ~ b) =>
     Bool -> Text -> b -> t a -> Sem r (Lens.IxValue (t a))
lookupError' showKeys msg k c = note (toS $ err <> toS msg) $ c Lens.^? Lens.ix k
  where err = shot (typeOf k) <> " not found: " <> shot k <> ". " <> asd
        asd | showKeys  = "Available:\n" <> unlines (shot <$> (c ^.. Lens.ifolded . Lens.asIndex))
            | True      = "Container count " <> shot (length c)
{-# INLINEABLE lookupError' #-}

lookupErrorNoKeys = lookupError' False
{-# INLINE lookupErrorNoKeys #-}
lookupErrorKeys = lookupError' True
{-# INLINE lookupErrorKeys #-}

dropAndLowerCaseNext :: Int -> [Char] -> [Char]
dropAndLowerCaseNext n = \case { [] -> []; h:t -> toLower h : t} . drop n
  
-- seqM :: Monad m => m b -> m b
-- seqM = chain pureM

-- -- | similar: Control.Exception.evaluate instead
-- pureM :: Applicative f => a -> f a
-- pureM x = seq x $ pure x


checkAbsError :: Members '[ErrorE, Reader a] r => ErrorMsg -> (a -> Double) -> Double -> Double -> Sem r ()
checkAbsError msg tolE expected actual = asks tolE >>= g
  where g tol = when (abs (actual - expected) > tol) $ throw err
          where err = msg <> "abs(" <> shot actual <> " - " <> shot expected <> ") > " <> shot tol

-- -- | evaluate to WHNF
-- evaluateEither :: HasCallStack => a -> Either Text a
-- evaluateEither a = unsafePerformIO $ E.catch (Right <$> E.evaluate a) $ \e -> pure $ Left $ shot (e :: SomeException) 

-- evaluateThrow :: (HasCallStack, TE r) => Text -> a -> Sem r a
-- evaluateThrow msg a = unsafePerformIO $ E.catch (pure <$> E.evaluate a) $ \e -> pure $ throw $ msg <> shot (e :: SomeException) 

-- evaluateLog :: forall a r . (HasCallStack, Monoid a, Member Log r) => Severity -> Text -> a -> Sem r a
-- evaluateLog sev msg a = unsafePerformIO $ E.catch (pure <$> E.evaluate a)
--   $ \e -> pure $ mempty <$ log sev (msg <> shot (e :: SomeException))

runH :: TE r => H.H a -> Sem r a
runH = fromEitherWith (shot) . H.runHEither

logH :: (Monoid a, Member Log r) => Severity -> Text -> H.H a -> Sem r a 
logH sev msg = H.runHWith (\e -> mempty <$ log sev (msg <> shot e)) pure
