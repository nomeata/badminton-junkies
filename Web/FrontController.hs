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
    redirectToUrl "https://badjunk.nomeata.de"
    sd <- getSession "userid" >>= \case
        Just uid -> do
            mbuser <- query @User
                |> filterWhere (#id, uid)
                |> fetchOneOrNothing
            case mbuser of
                Just user -> do
                    mafi <- getSession @(Id User) "acting_for_id"
                    case mafi of
                        Just other_user_id -> do
                            other_user <- fetchOneOrNothing other_user_id
                            pure $ Just $ SessionData user other_user
                        Nothing ->
                            pure $ Just $ SessionData user Nothing
                Nothing -> pure Nothing
        Nothing -> pure Nothing
    putContext sd
