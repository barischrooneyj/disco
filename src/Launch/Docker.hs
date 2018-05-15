{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Launch.Docker where

import           Data.Text
import           Data.Time          (UTCTime)
import           Servant.API
import           Servant.Server
import qualified System.Environment as Env

import           Network            (Message, NodeID)

launch :: IO ()
launch  = do
  args <- Env.getArgs
  putStrLn "foo"

-- | API for receiving a message.
type NodeAPI =
  "message" :> QueryParam "message" Message :> PostNoContent '[JSON] NoContent

type MessageHandler = Message -> IO ()

-- | Handler that runs the given function on message receipt.
recv :: MessageHandler -> Maybe Message -> Handler NoContent
recv f (Just message) = return NoContent

es :: MessageHandler -> Server NodeAPI
es = recv

send :: Message -> NodeID -> IO ()
send msg id = print "foo"
