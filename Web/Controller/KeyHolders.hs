module Web.Controller.KeyHolders where

import Web.Controller.Prelude
import Web.View.KeyHolders.Index

instance Controller KeyHoldersController where
    action KeyHoldersAction = do
        entries <- query @Keyholder
            |> orderByAsc #keyNumber
            |> fetch

        render (IndexView { .. })

    action ChangeKeyHolder = do
        needAuth

        let key_number = param @Int "keyNumber"
        let new_holder = param @Text "newHolder"

        entry <- query @Keyholder
            |> orderByAsc #keyNumber
            |> filterWhere (#keyNumber, key_number)
            |> fetchOne

        let entry' = entry |> set #holder new_holder
        entry' |> updateRecord

        setSuccessMessage $ "Key " <> show key_number <> " is now held by " <> new_holder
        logMessage $ "indicated that key number " <> show key_number <> " was passed from " <> (entry |> get #holder) <> " to " <> new_holder
        redirectTo KeyHoldersAction

needAuth :: (?context::ControllerContext) => IO ()
needAuth = fromContext @(Maybe SessionData) >>= \case
    Just sd -> pure ()
    Nothing -> err "Please log in first"

logMessage txt = do
    sd <- fromMaybe (error "This should not happen") <$> fromContext
    newRecord @Log |> set #text (userName (user sd) <> " " <> txt) |> createRecord

err msg = setErrorMessage msg >> redirectTo KeyHoldersAction >> pure undefined
ok msg = setSuccessMessage msg >> redirectTo KeyHoldersAction >> pure undefined
