module Web.Controller.Logs where

import Web.Controller.Prelude
import Web.View.Logs.Index

instance Controller LogsController where
    action LogsAction {..} = do
        fromContext @(Maybe SessionData) >>= \case
            Nothing -> err "Please log in first"
            Just sd -> pure ()

        (entryQ, pagination) <- query @Log
            |> orderByDesc #createdAt
            |> paginate

        entries <- entryQ |> fetch
        render (IndexView { .. })

err msg = setErrorMessage msg >> redirectTo RegistrationsAction >> pure undefined
