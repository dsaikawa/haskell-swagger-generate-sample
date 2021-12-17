{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings #-}
module Lib
  ( generateSwagger
  ) where
import           Control.Lens                   ( (&)
                                                , (.~)
                                                , (?~)
                                                , mapped
                                                )
import           Data.Aeson                     ( ToJSON(toJSON)
                                                , encode
                                                )
import qualified Data.ByteString.Lazy.Char8    as BL8
import           Data.Swagger                   ( HasDescription(description)
                                                , HasExample(example)
                                                , HasInfo(info)
                                                , HasRequired(required)
                                                , HasSchema(schema)
                                                , HasTitle(title)
                                                , HasVersion(version)
                                                , Swagger
                                                , ToSchema
                                                , defaultSchemaOptions
                                                , genericDeclareNamedSchema
                                                )
import           Data.Swagger.Internal.Schema   ( ToSchema(..)
                                                , genericDeclareNamedSchema
                                                )
import           Data.Typeable                  ( Typeable )
import           GHC.Generics                   ( Generic )
import           Servant                        ( type (:>)
                                                , Description
                                                , Get
                                                , JSON
                                                , Proxy(..)
                                                , Summary
                                                )
import           Servant.Swagger                ( HasSwagger(toSwagger) )

data User = User
  { userId    :: Integer
  , firstName :: String
  , lastName  :: String
  }
  deriving (Show, Generic, Typeable)

type API
  = Summary "path summary" :> Description "path description" :> "users" :> Get '[JSON] [User]


instance ToJSON User
instance ToSchema User where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      &  mapped
      .  schema
      .  description
      ?~ "Swagger Model description"
      &  mapped
      .  schema
      .  example
      ?~ toJSON (User 1 "firstname" "lastname")
      &  mapped
      .  schema
      .  required
      .~ ["userId"]

api :: Proxy API
api = Proxy

userSwagger :: Swagger
userSwagger =
  toSwagger (Proxy :: Proxy API)
    &  info
    .  title
    .~ "API Title"
    &  info
    .  version
    .~ "API Version"

generateSwagger :: IO ()
generateSwagger = BL8.writeFile "swagger.json" $ encode userSwagger
