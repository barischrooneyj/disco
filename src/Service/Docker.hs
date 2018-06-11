module Service.Docker where

import           Data.ByteString.Lazy.Char8 (unpack)
import           Data.Maybe                 (fromJust)
import           Data.Text                  (pack)
import           Docker.Client.Api          (createContainer, getDockerVersion)
import           Docker.Client.Http         (defaultHttpHandler, runDockerT)
import           Docker.Client.Types        (CreateOpts (..), defaultClientOpts,
                                             defaultContainerConfig,
                                             defaultHostConfig)
import qualified System.Directory           as Dir
import           System.Exit                (ExitCode (..))
import           System.FilePath            ((</>))
import qualified System.Process.Typed       as Proc

import           Network                    (Node (..), Service (..))

-- | A service that runs nodes in a local Docker container.
--
-- When using this service the Docker daemon must be accessible over TCP on port
-- 2375. Due to a bug on Docker for macOS (below) you will have to run:
--
--   socat TCP-LISTEN:2375,reuseaddr,fork,bind=127.0.0.1 UNIX-CLIENT:/var/run/docker.sock
--
--   https://github.com/docker/for-mac/issues/1156#issuecomment-273764881
localDocker :: Network.Service
localDocker = Service { _startNodes = startNodesOld }

startNodes :: [Node] -> IO ()
startNodes nodes = do
  h       <- defaultHttpHandler

  -- | Example running a Docker command.
  version <- runDockerT (defaultClientOpts, h) getDockerVersion
  putStrLn $ "\nVersion: " ++ show version
  mapM_ (startNode h) nodes

  where startNode httpHandler node = do
          let containerConfig' = defaultContainerConfig $ pack "disco-docker"
              containerName = pack $ "disco-docker" ++ show (fromJust $ _identifier node)
              createOpts = CreateOpts containerConfig' defaultHostConfig
              createCommand = createContainer createOpts $ Just containerName
          container <- runDockerT (defaultClientOpts, httpHandler) createCommand
          putStrLn $ "\nCommand: " ++ show createOpts
          putStrLn $ "\nContainer: " ++ show container

-- ** OLD!! Docker Compose code.

-- | Launch a complete (fully connected) network topology of nodes.
startNodesOld :: [Node] -> IO ()
startNodesOld nodes = do
  let dockerfile =
        foldl dockerfileWithNode (unlines ["version: '3'", "", "services:"]) nodes
  outFile <- (</> "cs/disco/docker-compose.yml") <$> Dir.getHomeDirectory
  writeFile outFile dockerfile
  print $ "Saved Dockerfile to: " ++ outFile
  (e, stdout, stderr) <- Proc.readProcess $
    Proc.proc "docker-compose" ["up"]
  putStrLn $ unpack $ case e of { ExitSuccess -> stdout; ExitFailure _ -> stderr }

-- | A Dockerfile string with a node added.
dockerfileWithNode :: String -> Node -> String
dockerfileWithNode dockerfile node =
  dockerfile ++ unlines [
      "  disco-docker-" ++ show (fromJust $ _identifier node) ++ ":",
      "    image: disco-docker",
      "    environment:",
      "      - DISCO_PROGRAM=" ++ show (_program node),
      "    command: /usr/local/bin/disco-docker-exe",
      "    tty: true"
      ]

-- | Service as a typeclass rather than a data type ? -----------
-- NOTE: Unused.
data LocalDocker

class Service s where
  _runNodes :: s -> [Node] -> IO ()

instance Service.Docker.Service LocalDocker where
  _runNodes = const startNodesOld
-- | ------------------------------------------------------------
