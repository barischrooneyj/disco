{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Messaging where

import           Servant.API

-- * Instances of the Messaging data type.

type UserAPI = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name

data User = User {
  name :: String,
  age  :: Int
}
