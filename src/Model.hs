{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Acid (Query, Update, makeAcidic)
import Data.SafeCopy (base, deriveSafeCopy)
import Data.Text (Text)
import qualified Data.IntMap as IntMap
import Data.IntMap (IntMap)
import Data.Typeable (Typeable)
import Data.Aeson (ToJSON, FromJSON, (.:), (.=), parseJSON, toJSON, Value(..), object)
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Data.HashMap.Strict (union)

data Entity a b = Entity a b deriving (Show, Typeable)

data Coworker = Coworker {
  coworkerFirstname :: Text,
  coworkerLastname :: Text,
  coworkerEmail :: Text
  } deriving (Show, Typeable)


instance (ToJSON a, ToJSON b) => ToJSON (Entity a b) where
  toJSON (Entity k v) = object ["id" .= k] `merge` toJSON v
         

merge :: Value -> Value -> Value
merge (Object h) (Object h') = Object (h `union` h')
merge _ _ = error "Can only merge objects"

instance FromJSON Coworker where
  parseJSON (Object s) = Coworker <$>
                         s .: "firstname" <*>
                         s .: "lastname" <*>
                         s .: "email"
  parseJSON _ = mzero


instance ToJSON Coworker where
  toJSON Coworker{..} = object [ "firstname" .= coworkerFirstname
                               , "lastname" .= coworkerLastname
                               , "email" .= coworkerEmail
                               ]


data PersistentState = PersistentState {
     coworkers :: IntMap Coworker
  } deriving (Show, Typeable)
             
deriveSafeCopy 0 'base ''Coworker
deriveSafeCopy 0 'base ''PersistentState
deriveSafeCopy 0 'base ''Entity


findAllCoworkers :: Query PersistentState [Entity IntMap.Key Coworker]
findAllCoworkers = do
  state <- ask
  return $ map (uncurry  Entity) . IntMap.toAscList $ coworkers state


addCoworker :: Coworker -> Update PersistentState IntMap.Key
addCoworker coworker = do
  PersistentState coworkerDB <- get
  let newKey = case IntMap.maxViewWithKey coworkerDB of
        Just ((lastKey, _), _) -> lastKey + 1
        Nothing -> 1
  put $ PersistentState (IntMap.insert newKey coworker coworkerDB)
  return newKey


makeAcidic ''PersistentState ['addCoworker, 'findAllCoworkers]


