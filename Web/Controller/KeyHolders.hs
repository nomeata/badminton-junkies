module Web.Controller.KeyHolders where

import Web.Controller.Prelude
import Web.View.KeyHolders.Index

instance Controller KeyHoldersController where
    action KeyHoldersAction = do
        users <- query @User
            |> orderByAsc #fullname
            |> fetch
        entries <- query @Keyholder
            |> orderByAsc #keyNumber
            |> fetch

        render (IndexView { .. })

    action ChangeKeyHolder = do
        needAuth

        let key_number = param @Int "keyNumber"
        new_holder <- fetch $ param @(Id User) "newHolder"

        entry <- query @Keyholder
            |> orderByAsc #keyNumber
            |> filterWhere (#keyNumber, key_number)
            |> fetchOne
        entry' <- entry |> fetchRelated #userId

        entry |> set #userId new_holder.id
              |> updateRecord

        setSuccessMessage $ "Key " <> show key_number <> " is now held by " <> userName new_holder
        logMessage $ "indicated that key number " <> show key_number <> " was passed from " <> (userName entry'.userId) <> " to " <> userName new_holder
        redirectTo KeyHoldersAction

needAuth :: (?context::ControllerContext) => IO ()
needAuth = fromContext @(Maybe SessionData) >>= \case
    Just sd -> pure ()
    Nothing -> err "Please log in first"

err msg = setErrorMessage msg >> redirectTo KeyHoldersAction >> pure undefined
ok msg = setSuccessMessage msg >> redirectTo KeyHoldersAction >> pure undefined
