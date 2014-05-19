{-# LANGUAGE OverloadedStrings #-}

module Site(app) where

import           Data.ByteString (ByteString)
import qualified Data.Text as T
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileServe
import Snap.Snaplet.AcidState (query, update, acidInit)
import Snap.Extras.JSON (writeJSON, getJSON)
import           Application
import qualified Data.IntMap as IntMap
import Model (PersistentState (PersistentState), FindAllCoworkers(FindAllCoworkers), AddCoworker(AddCoworker))
import Data.Aeson ((.=), object)

handleAddCoworker :: AppHandler ()
handleAddCoworker = do
  eitherCoworker <- getJSON
  case eitherCoworker of
    Left e -> writeText $ T.pack e
    Right coworker -> do
      coworkerId <- update (AddCoworker coworker)
      writeJSON $ object [ "key" .= (T.pack . show) coworkerId]
      
handleFindAllCoworkers :: AppHandler ()
handleFindAllCoworkers = do
  coworkers <- query FindAllCoworkers
  writeJSON coworkers

routes :: [(ByteString, AppHandler ())]
routes = [ ("coworkers", method POST handleAddCoworker)
         , ("coworkers", method GET handleFindAllCoworkers)
         , ("/static", serveDirectory "src/static")
         ]

app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    addRoutes routes
    as <- nestSnaplet "acid" acid $ acidInit (PersistentState IntMap.empty)
    return $ App as

