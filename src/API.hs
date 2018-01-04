{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module API (
  CoinMarketCapAPI
  ) where

import           Servant.API

import           Types

type CoinMarketCapAPI =
       "ticker" :> Capture "id" String :> Get '[JSON] Tickers
  :<|> "ticker" :> Get '[JSON] Tickers
