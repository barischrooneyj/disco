{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Listen where

import           Servant.API

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name              :: String,
  age               :: Int
}
