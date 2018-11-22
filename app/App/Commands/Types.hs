module App.Commands.Types
  ( CreateIndexOptions(..)
  ) where

data CreateIndexOptions = CreateIndexOptions
  { _createIndexOptionsFilePath     :: FilePath
  , _createIndexOptionsOutputIbFile :: Maybe FilePath
  , _createIndexOptionsOutputBpFile :: Maybe FilePath
  } deriving (Eq, Show)
