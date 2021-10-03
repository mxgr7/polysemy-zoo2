{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Zoo.SqlServer
  (module Polysemy.Zoo.SqlServer
  ,Pool
  ,Connection
  )where


import Data.Pool
import Database.ODBC.SQLServer
import Polysemy.Zoo.Prelude
import Polysemy.Zoo.Utils
import System.IO.Error

data SqlServer m a where
  SqlServerQuery    :: FromRow a => (SqlServerSchemaName -> Query) -> SqlServer m [a]
  SqlServerExec     ::              (SqlServerSchemaName -> Query) -> SqlServer m ()

newtype SqlServerSchemaName = SqlServerSchemaName { fromSqlServerSchemaName :: Text }

makeSem ''SqlServer

data SqlServerConfig = SqlServerConfig  { scDsn                 :: Text
                                        , scPoolSize            :: Word
                                        , scPoolTtl             :: Word
                                        , scSchemaName          :: SqlServerSchemaName
                                        , scVersionCheck        :: Maybe (Query, Text)
                                        -- ^ query should yield a list of 0 or one version strings
                                        }

  
-- * Query builders
in_ :: (ToSql a, Foldable f) => f a -> Query
in_ x = rut " in (" <> mconcat (intersperse (rut ",") $ toSql <$> toList x) <> rut ") "
  where rut = rawUnescapedText

-- * Interpreters

runSqlServerEmbedIO :: Members '[Log, Embed IO, ErrorE] r => SqlServerConfig
  -> Sem (SqlServer ': Reader (Pool Connection) ': r) a -> Sem r a
runSqlServerEmbedIO SqlServerConfig{..} ac = do
  pool <- embed $ newPool $ defaultPoolConfig newCon close (fromIntegral scPoolTtl) (fromIntegral scPoolSize)
  runReader pool $ runSqlServerAsPoolReader scSchemaName ac
  where newCon = do
          conn <- connect scDsn
          forM scVersionCheck $ \(qu, v) -> do
            res <- query conn qu
            when ([v] /= res) $ ioError $ userError $ "SqlServer version mismatch. Expected '"  <>
              toS v <> "'. Got: " <> show res
          pure conn 

runSqlServerAsPoolReader :: Members '[Log, ErrorE, Embed IO, Reader (Pool Connection)] r
  => SqlServerSchemaName -> Sem (SqlServer ': r) a -> Sem r a
runSqlServerAsPoolReader sn = interpret $ \case
  SqlServerQuery        qu -> withConnection query $ qu sn
  SqlServerExec         qu -> withConnection exec $ qu sn
  where withConnection :: Members '[Log, Embed IO, ErrorE, Reader (Pool Connection)] r
                       => (Connection -> Query -> IO a) -> Query -> Sem r a
        withConnection act q = do
          log Trace "Asking SqlServer Pool for connection ..."
          embedCatch . flip withResource (flip act q) =<< ask


-- versionCheck :: Text -> IO ()
