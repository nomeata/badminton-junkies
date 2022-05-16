module Web.Controller.Logs where

import Web.Controller.Prelude
import Web.View.Logs.Index

instance Controller LogsController where
    action LogsAction {..} = do
        entries <- query @Log |> orderByDesc #createdAt |> fetch
        render (IndexView { .. })
