module Web.Controller.Logs where

import Web.Controller.Prelude
import Web.View.Logs.Index

instance Controller LogsController where
    action LogsAction {..} = do
        fromContext @(Maybe SessionData) >>= \case
            Nothing -> err "Please log in first"
            Just sd -> pure ()

        entries <- query @Log |> orderByDesc #createdAt |> fetch
        render (IndexView { .. })

err msg = setErrorMessage msg >> redirectTo RegistrationsAction >> pure undefined
