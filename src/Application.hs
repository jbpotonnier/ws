{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Application where

import Control.Lens
import Snap.Snaplet
import Model (PersistentState)

import Snap.Snaplet.AcidState (Acid, HasAcid (getAcidStore))

data App = App
    { _acid :: Snaplet (Acid PersistentState)}

makeLenses ''App

instance HasAcid App PersistentState where
    getAcidStore = view (acid.snapletValue)

type AppHandler = Handler App App


