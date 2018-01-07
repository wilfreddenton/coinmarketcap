module Core where

import           Network.HTTP.Client     (Manager, newManager)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant.Client

import           Client
import           Types

manager :: IO Manager
manager = newManager tlsManagerSettings

clientEnv :: IO ClientEnv
clientEnv = do
  manager' <- manager
  return (ClientEnv manager' (BaseUrl Https "api.coinmarketcap.com" 443 "/v1"))

getTicker' :: TickerId -> Maybe FiatSymbol -> ClientM Ticker
getTicker' tickerId symbol = do
  tickers <- getTicker tickerId symbol
  return $ head tickers

tickers :: Maybe Integer -> Maybe Integer -> Maybe FiatSymbol -> IO (Either ServantError Tickers)
tickers start limit symbol = do
  clientEnv' <- clientEnv
  runClientM (getTickers start limit symbol) clientEnv'

ticker :: TickerId -> Maybe FiatSymbol -> IO (Either ServantError Ticker)
ticker tickerId symbol = do
  clientEnv' <- clientEnv
  runClientM (getTicker' tickerId symbol) clientEnv'

globalData :: Maybe FiatSymbol -> IO (Either ServantError GlobalData)
globalData symbol = do
  clientEnv' <- clientEnv
  runClientM (getGlobalData symbol) clientEnv'
