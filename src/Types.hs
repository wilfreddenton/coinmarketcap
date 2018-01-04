{-# LANGUAGE OverloadedStrings #-}

module Types (
  Ticker,
  Tickers
  ) where

import           Data.Aeson

data Ticker = Ticker
  { tId               :: String,
    tName             :: String,
    tSymbol           :: String,
    tRank             :: String,
    tPriceUSD         :: String,
    tPriceBTC         :: String,
    t24HVolumeUSD     :: String,
    tMarketCapUSD     :: String,
    tAvailableSupply  :: String,
    tTotalSupply      :: String,
    tMaxSupply        :: String,
    tPercentChange1H  :: String,
    tPercentChange24H :: String,
    tPercentChange7D  :: String,
    tLastUpdated      :: String
  } deriving (Eq, Show)

instance FromJSON Ticker where
  parseJSON (Object o) =
    Ticker <$> o .: "id"
           <*> o .: "name"
           <*> o .: "symbol"
           <*> o .: "rank"
           <*> o .: "price_usd"
           <*> o .: "price_btc"
           <*> o .: "24h_volume_usd"
           <*> o .: "market_cap_usd"
           <*> o .: "available_supply"
           <*> o .: "total_supply"
           <*> o .: "max_supply"
           <*> o .: "percent_change_1h"
           <*> o .: "percent_change_24h"
           <*> o .: "percent_change_7d"
           <*> o .: "last_updated"

type Tickers = [Ticker]

