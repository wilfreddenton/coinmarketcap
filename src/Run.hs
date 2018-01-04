module Run where

import           Network.HTTP.Client     (newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client

import           Client
import           Types

ticker :: ClientM Ticker
ticker = do
  tickers <- getTicker "cardano"
  return $ head tickers

run :: IO ()
run = do
  manager <- newManager tlsManagerSettings

  res <- runClientM ticker (ClientEnv manager (BaseUrl Https "api.coinmarketcap.com" 443 "/v1"))
  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right t -> do
      print t

