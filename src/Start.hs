module Start where

-- * Start networks and nodes on services.

import           Data.Maybe           (fromJust)
import           Data.Text            (pack)
import           Docker.Client.Api    (createContainer, getDockerVersion)
import           Docker.Client.Http   (defaultHttpHandler, runDockerT)
import           Docker.Client.Types  (CreateOpts (..), apiVer, baseUrl,
                                       defaultClientOpts,
                                       defaultContainerConfig,
                                       defaultHostConfig)
import qualified System.Directory     as Dir
import           System.Exit          (ExitCode (..))
import           System.FilePath      ((</>))
import qualified System.Process.Typed as Proc

import           Network              (Edges (..), Network (..), Node (..),
                                       Service (..))
import           Util                 (applyUniqueIDs)


-- | Start the given network!
startNetwork :: Network -> IO ()
startNetwork network = do
  let uniqueNodeNetwork = applyUniqueIDs network
  case _edges uniqueNodeNetwork of
    CompleteGraph   -> startCompleteGraph $ _nodes uniqueNodeNetwork
    Ring            -> print "NOTE: Ring not implemented"
    Edges _setEdges -> print "NOTE: Set of edges not implemented"


startCompleteGraph :: [Node] -> IO ()
startCompleteGraph nodes = do
  let nodeIds = ["node-" ++ show (fromJust $ _id node) | node <- nodes]
      clientOpts = defaultClientOpts { apiVer = pack "v1.37" }
  print nodeIds
  h <- defaultHttpHandler
  version <- runDockerT (clientOpts, h) getDockerVersion
  print version

  -- We map startNode over each node, startnode is currently trying to communicate
  -- with the docker daemon, this needs to be debugged, looking at response/request.

  mapM_ (startNode h) nodes

  where startNode httpHandler _unused_node = do
          let containerConfig' = defaultContainerConfig $ pack "disco-docker"
              -- containerName = pack $ "disco-docker" ++ show (fromJust $ _id node)
              clientOpts = defaultClientOpts {
                apiVer = pack "v1.37",
                baseUrl = pack "/v.137"
                                             }
              createOpts = CreateOpts containerConfig' defaultHostConfig
              createCommand = createContainer createOpts Nothing -- (Just $ containerName)
          container <- runDockerT (clientOpts, httpHandler) createCommand
          print container

-- ** Old Docker Compose code.

-- | Assign IDs to each node, if they don't have one.
dockerfileHead :: String
dockerfileHead = unlines ["version: '3'", "", "services:"]


-- | Launch a complete (fully connected) network topology of nodes.
startCompleteGraph_ :: [Node] -> IO ()
startCompleteGraph_ nodes = do
  let dockerfile = foldl dockerfileWithNode dockerfileHead nodes
  outFile <- (</> "cs/disco/docker-compose.yml") <$> Dir.getHomeDirectory
  writeFile outFile dockerfile
  print $ "Saved Dockerfile to: " ++ outFile
  (e, stdout, stderr) <- Proc.readProcess $
    Proc.proc "docker-compose" ["up"]
  print $ case e of { ExitSuccess -> stdout; ExitFailure _ -> stderr }


-- | A Dockerfile string with a node added.
--
-- We expect the node to have an ID.
dockerfileWithNode :: String -> Node -> String
dockerfileWithNode dockerfile node =
  case _service node of
    LocalDocker -> dockerfile ++ unlines [
      "  disco-docker-" ++ show (fromJust $ _id node) ++ ":",
      "    image: disco-docker",
      "    environment:",
      "      - DISCODOCKERLAUNCH=" ++ show (_exe node),
      "    command: /usr/local/bin/disco-docker-exe",
      "    tty: true"
      ]


