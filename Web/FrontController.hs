module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Registrations
import Web.Controller.Logs
import Web.Controller.Sessions
import Web.Controller.Static

instance FrontController WebApplication where
    controllers =
        [ startPage RegistrationsAction
        -- Generator Marker
        , parseRoute @RegistrationsController
        , parseRoute @LogsController
        , parseRoute @SessionsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
        initUser

initUser :: (?context :: ControllerContext, ?modelContext :: ModelContext) => IO ()
initUser = do
    mbn <- getSession @Text "name"
    putContext (UserFullName mbn)
