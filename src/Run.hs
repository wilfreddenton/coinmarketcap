module Run where

import           Data.Aeson
import qualified Data.ByteString.Lazy    as B
import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client

import           Client
import           Core
import           Types

run :: IO ()
run = do
  res <- globalData Nothing
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right t -> do
      B.writeFile "out.json" $ encode t

