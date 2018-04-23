module Launch.Docker where

import qualified System.Environment as Env

launch :: IO ()
launch  = do
  args <- Env.getArgs
  print "foo"
