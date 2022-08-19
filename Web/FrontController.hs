module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Registrations
import Web.Controller.Logs
import Web.Controller.KeyHolders
import Web.Controller.Sessions
import Web.Controller.Static

instance FrontController WebApplication where
    controllers =
        [ startPage RegistrationsAction
        -- Generator Marker
        , parseRoute @RegistrationsController
        , parseRoute @LogsController
        , parseRoute @KeyHoldersController
        , parseRoute @SessionsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initUser

initUser :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO ()
initUser = do
    sd <- getSession "userid" >>= \case
        Just uid -> do
            mbuser <- query @User
                |> filterWhere (#id, uid)
                |> fetchOneOrNothing
            case mbuser of
                Just user -> do
                    maf <- getSession @Text "acting_for"
                    pure $ Just $ SessionData user maf
                Nothing -> pure Nothing
        Nothing -> pure Nothing
    putContext sd
