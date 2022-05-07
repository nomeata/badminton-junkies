module Web.FrontController where

import IHP.RouterPrelude
import Web.Controller.Prelude
import Web.View.Layout (defaultLayout)

-- Controller Imports
import Web.Controller.Registrations
import Web.Controller.Logs
import Web.Controller.Static

instance FrontController WebApplication where
    controllers =
        [ startPage RegistrationsAction
        -- Generator Marker
        , parseRoute @RegistrationsController
        , parseRoute @LogsController
        ]

instance InitControllerContext WebApplication where
    initContext = do
        setLayout defaultLayout
        initAutoRefresh
