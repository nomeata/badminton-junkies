module Web.Controller.Logs where

import Web.Controller.Prelude
import Web.View.Registrations.Index

instance Controller LogsController where
    action LogsAction {..} = do
        err "TODO"

err msg = setErrorMessage msg >> redirectTo RegistrationsAction
ok msg = setSuccessMessage msg >> redirectTo RegistrationsAction
