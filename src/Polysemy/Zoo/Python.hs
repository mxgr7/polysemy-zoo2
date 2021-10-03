{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Zoo.Python where

import           Hoff
import           Polysemy.Log
import           Polysemy.Zoo.FilesystemCache
import           Polysemy.Zoo.Utils
import qualified Prelude as P

type CommandLineArgs = [String]

data Python m a where
  PythonByteString      :: FsCacheKey -> CommandLineArgs -> Maybe ByteString -> Text -> Python m ByteString
  PythonTable           :: FsCacheKey -> CommandLineArgs -> Maybe Table      -> Text -> Python m Table
  PythonTables          :: FsCacheKey -> CommandLineArgs -> Maybe Table      -> Text -> Python m [(Text, Table)]

data PythonConfig =  PythonConfig       { cPythonBin    :: FilePath
                                        , cPythonPath   :: String
                                        }

makeSem ''Python

runPythonInEmbedIO :: Members '[FilesystemCache, Embed IO, ErrorE, Reader PythonConfig] r => Sem (Python ': r) a -> Sem r a
runPythonInEmbedIO = interpret $ \case
  PythonByteString      key args input source   -> embedPython input source key args pythonEvalByteString
  PythonTable           key args input source   -> fromEither . cborToTable =<<
    embedPython input (singleTableSourceMod source <> renameOutputDfColumns) key args pythonEvalHoffCbor
  PythonTables          key args input source   -> fromEither . cborToTables =<<
    embedPython input (source <> renameOutputDfColumns) key args pythonEvalHoffCbor


embedPython :: Members '[Embed IO, ErrorE, FilesystemCache, Reader PythonConfig] r 
  => Maybe a -> Text -> FsCacheKey -> CommandLineArgs
  -> (FilePath -> Text -> [String] -> Maybe [(String, String)] -> Maybe a -> IO ByteString) -> Sem r ByteString
embedPython input source ck args eval = let ppEnv = "PYTHONPATH" in
  ask >>= \PythonConfig{..} -> fsCachedByteString ck $ embedCatch $ do
  env <- getEnvironment
  eval cPythonBin source args (Just $ P.filter (\(n,_) -> n /= ppEnv) env ++ [(ppEnv, cPythonPath)]) input

-- | rename to camel case
-- TODO this will leave the columns unchanged if renaming introduces duplicate columns
renameOutputDfColumns :: Text
renameOutputDfColumns = [q|
import stringcase

for _,outputDf in outputDfs:

  renamed = [stringcase.camelcase(x.lower()) for x in outputDf]
  
  if 0:
    print("renaming columns from:\n", list(outputDf))
    print("to:\n", renamed)
  
  if len(set(renamed)) == len(set(outputDf.columns)):
    outputDf.columns = renamed
|]


runPythonInIONoCache :: PythonConfig -> Sem '[Python, FilesystemCache, Reader PythonConfig, Log, ErrorE , GhcTime, Embed IO, Final IO] a
  -> IO a
runPythonInIONoCache config = runFinal . embedToFinal . interpretTimeGhc . errorToIOFinalThrow . interpretLogStderr . runReader config .
  runFilesystemCache "/unused_cache_folder" mempty mempty . runPythonInEmbedIO

