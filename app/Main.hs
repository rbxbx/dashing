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

data RandomData a = RandomData
    { x :: a
    , y :: a
    } deriving (Eq, Show, Generic, FromJSON, ToJSON)

-- * API Definition -------------------------------------------------------------

type API =                Get '[JSON] (RandomData Int)
        :<|> "randoms" :> Get '[JSON] [RandomData Int]
        :<|> "dubs" :> Get '[JSON] [RandomData Double]
        :<|> "strings" :> Get '[JSON] [RandomData Char]

randomApi :: Proxy API
randomApi = Proxy

handleGet :: Handler (RandomData Int)
handleGet = return $ RandomData 2 1

genRands :: Random a => IO [RandomData a]
genRands = do
    z <- randomRIO (0, 1000)
    xs <- rands z
    ys <- rands z
    return $ uncurry RandomData <$> zip xs ys
  where rands x = replicateM x randomIO

handleRandoms :: Handler [RandomData Int]
handleRandoms = lift genRands

handleDubs :: Handler [RandomData Double]
handleDubs = lift genRands

handleStrings :: Handler [RandomData Char]
handleStrings = lift genRands

-- * Server Definition ----------------------------------------------------------

apiServer :: Server API
apiServer = handleGet
       :<|> handleRandoms
       :<|> handleDubs
       :<|> handleStrings

main :: IO ()
main = run 8001 (serve randomApi apiServer)
