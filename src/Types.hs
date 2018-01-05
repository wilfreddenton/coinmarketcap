{-# LANGUAGE OverloadedStrings #-}

module Types (
  TickerId,
  Ticker,
  Tickers,
  GlobalData,
  FiatSymbol (..)
  ) where

import           Data.Aeson
import           Data.Maybe
import           Data.Text                hiding (filter, map)
import           Web.Internal.HttpApiData

type TickerId = String

data Ticker = Ticker
  { tId               :: TickerId
  , tName             :: String
  , tSymbol           :: String
  , tRank             :: String
  , tPriceBtc         :: String
  , tPriceUsd         :: String
  , tPriceJpy         :: Maybe String
  , t24HVolumeUsd     :: String
  , tMarketCapUsd     :: String
  , tMarketCapJpy     :: Maybe String
  , tAvailableSupply  :: String
  , tTotalSupply      :: String
  , tMaxSupply        :: Maybe String
  , tPercentChange1h  :: String
  , tPercentChange24h :: String
  , tPercentChange7d  :: String
  , tLastUpdated      :: String
  } deriving (Eq, Show)

instance FromJSON Ticker where
  parseJSON (Object o) =
    Ticker <$> o .: "id"
           <*> o .: "name"
           <*> o .: "symbol"
           <*> o .: "rank"
           <*> o .: "price_btc"
           <*> o .: "price_usd"
           <*> o .:? "price_jpy"
           <*> o .: "24h_volume_usd"
           <*> o .: "market_cap_usd"
           <*> o .:? "market_cap_jpy"
           <*> o .: "available_supply"
           <*> o .: "total_supply"
           <*> o .: "max_supply"
           <*> o .: "percent_change_1h"
           <*> o .: "percent_change_24h"
           <*> o .: "percent_change_7d"
           <*> o .: "last_updated"

instance ToJSON Ticker where
  toJSON (Ticker tickerId name symbol rank priceBtc priceUsd
          priceJpy twoFourHVolumeUsd marketCapUsd marketCapJpy
          availableSupply totalSupply maxSupply percentChange1h
          percentChange24h percentChange7d lastUpdated
         ) =
    let base_object = [ "id" .= tickerId
                             , "name" .= name
                             , "symbol" .= symbol
                             , "rank" .= rank
                             , "price_btc" .= priceBtc
                             , "price_usd" .= priceUsd
                             , "24h_volume_usd" .= twoFourHVolumeUsd
                             , "market_cap_usd" .= marketCapUsd
                             , "available_supply" .= availableSupply
                             , "total_supply" .= totalSupply
                             , "max_supply" .= maxSupply
                             , "percent_change_1h" .= percentChange1h
                             , "percent_change_24h" .= percentChange24h
                             , "percent_change_7d" .= percentChange7d
                             , "last_updated" .= lastUpdated
                             ]
        optional_pairs = [("price_jpy", priceJpy), ("market_cap_jpy", marketCapJpy)]
        filterNothing = filter (\(_, v) -> not $ isNothing v)
        tuplesToPairs = map (\(k, v) -> k .= v)
     in object $ base_object ++ (tuplesToPairs . filterNothing) optional_pairs

type Tickers = [Ticker]

data GlobalData = GlobalData
  { gTotalMarketCapUsd            :: Integer
  , gTotal24hVolumeUsd            :: Integer
  , gBitcoinPercentageOfMarketCap :: Float
  , gActiveCurrencies             :: Integer
  , gLastUpdated                  :: Integer
  , gTotalMarketCapJpy            :: Maybe Integer
  , gTotal24hVolumeJpy            :: Maybe Integer
  }

instance FromJSON GlobalData where
  parseJSON (Object o) =
    GlobalData <$> o .: "total_market_cap_usd"
               <*> o .: "total_24h_volume_usd"
               <*> o .: "bitcoin_percentage_of_market_cap"
               <*> o .: "active_currencies"
               <*> o .: "last_updated"
               <*> o .: "total_market_cap_jpy"
               <*> o .: "total_24h_volume_jpy"

instance ToJSON GlobalData where
  toJSON (GlobalData totalMarketCapUsd total24hVolumeUsd bitcoinPercentageOfMarketCap
          activeCurrencies lastUpdated totalMarketCapJpy total24hVolumeJpy
         ) = object [ "total_market_cap_usd" .= totalMarketCapUsd
                    , "total_24h_volume_usd" .= total24hVolumeUsd
                    , "bitcoin_percentage_of_market_cap" .= bitcoinPercentageOfMarketCap
                    , "active_currencies" .= activeCurrencies
                    , "last_updated" .= lastUpdated
                    , "total_market_cap_jpy" .= totalMarketCapJpy
                    , "total_24h_volume_jpy" .= total24hVolumeJpy
                    ]

data FiatSymbol = AUD | BRL | CAD | CHF | CLP | CNY
                | CZK | DKK | EUR | GBP | HKD | HUF
                | IDR | ILS | INR | JPY | KRW | MXN
                | MYR | NOK | NZD | PHP | PKR | PLN
                | RUB | SEK | SGD | THB | TRY | TWD
                | ZAR deriving (Eq, Show)

instance ToHttpApiData FiatSymbol where
  toQueryParam = pack . show
