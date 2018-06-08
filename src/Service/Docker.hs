module Service.Docker where

import           Data.Maybe          (fromJust)
import           Data.Text           (pack)
import           Docker.Client.Api   (createContainer, getDockerVersion)
import           Docker.Client.Http  (defaultHttpHandler, runDockerT)
import           Docker.Client.Types (CreateOpts (..), defaultClientOpts,
                                      defaultContainerConfig, defaultHostConfig)

import           Network             (Node (..), Service (..))

-- | A service that runs nodes in a local Docker container.
--
-- When using this service the Docker daemon must be accessible over TCP on port
-- 2375. Due to a bug on Docker for macOS (below) you will have to run:
--
--   socat TCP-LISTEN:2375,reuseaddr,fork,bind=127.0.0.1 UNIX-CLIENT:/var/run/docker.sock
--
--   https://github.com/docker/for-mac/issues/1156#issuecomment-273764881
localDocker :: Service
localDocker = Service { _startNodes = startNodes }

startNodes :: [Node] -> IO ()
startNodes nodes = do
  h       <- defaultHttpHandler

  -- | Example running a Docker command.
  version <- runDockerT (defaultClientOpts, h) getDockerVersion
  putStrLn $ "\nVersion: " ++ show version
  mapM_ (startNode h) nodes

  where startNode httpHandler node = do
          let containerConfig' = defaultContainerConfig $ pack "disco-docker"
              containerName = pack $ "disco-docker" ++ show (fromJust $ _id node)
              createOpts = CreateOpts containerConfig' defaultHostConfig
              createCommand = createContainer createOpts $ Just containerName
          container <- runDockerT (defaultClientOpts, httpHandler) createCommand
          putStrLn $ "\nCommand: " ++ show createOpts
          putStrLn $ "\nContainer: " ++ show container
