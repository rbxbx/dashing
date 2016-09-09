{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveAnyClass    #-}

module Main where

import           GHC.Generics
import           Data.Aeson
import           Data.Text            (Text)
import           Data.Proxy
import           Servant.API
import           Servant.Server       (Handler, Server, serve)
import           Network.Wai
import           Network.Wai.Handler.Warp
import           System.Random
import           Control.Monad.Trans.Class
import           Control.Monad        (replicateM)

data RandomData
  = RandomData
    { x :: Int
    , y :: Int
    } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- * API Definition -------------------------------------------------------------

type API = Get '[JSON] RandomData
        :<|> "randoms" :> Get '[JSON] [RandomData]


randomApi :: Proxy API
randomApi = Proxy

handleGet :: Handler RandomData
handleGet = return $ RandomData 2 1

handleRandoms :: Handler [RandomData]
handleRandoms = do
        xs <- lift $ replicateM 10 randomIO
        ys <- lift $ replicateM 10 randomIO
        let zs = zip xs ys
        return uncurry RandomData <$> zs

-- * Server Definition ----------------------------------------------------------

apiServer :: Server API
apiServer = handleGet
          :<|> handleRandoms

main :: IO ()
main = run 8001 (serve randomApi apiServer)
