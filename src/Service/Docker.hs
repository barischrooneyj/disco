module Service.Docker where

import           Data.Maybe          (fromJust)
import           Data.Text           (pack)
import           Docker.Client.Api   (createContainer, getDockerVersion)
import           Docker.Client.Http  (defaultHttpHandler, runDockerT)
import           Docker.Client.Types (CreateOpts (..), apiVer, baseUrl,
                                      defaultClientOpts, defaultContainerConfig,
                                      defaultHostConfig)

import           Network             (Node (..), Service (..))

-- | Have not yet moved implementation to here.
-- Currently in Start.hs.
localDocker :: Service
localDocker = Service { startNodes = startNodes' }

startNodes' :: [Node] -> IO ()
startNodes' nodes = do
  let nodeIds = ["node-" ++ show (fromJust $ _id node) | node <- nodes]
      clientOpts = defaultClientOpts -- { apiVer = pack "v1.37" }
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

