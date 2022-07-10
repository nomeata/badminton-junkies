module Web.Controller.Sessions where

import Application.BuhlAuth

import Web.Controller.Prelude
import Web.View.Sessions.Edit

instance Controller SessionsController where
    action EditSessionAction = do
        let login_data = newRecord @LoginData
        render EditView { .. }

    action CreateSessionAction = do
        let login_data = newRecord @LoginData
              |> fill @["email","password"]
              |> validateField #email isEmail

        flip ifValid login_data $ \case
            Left login_data ->
                render EditView { .. }
            Right login_data -> do
                buhlLogin (get #email login_data) (get #password login_data) >>= \case
                    Left msg -> do
                        setErrorMessage $"Login failed: " <> msg
                        render EditView { .. }
                    Right name -> do
                        setSuccessMessage $ "Welcome, " <> name
                        setSession "name" name
                        redirectTo RegistrationsAction

    action ChangeNameAction = do
        let new_name = param @Text "nickname"
        setSuccessMessage $ "You are now known as " <> new_name
        setSession "nickname" new_name
        redirectTo RegistrationsAction

    action ChangeActingAction = do
        let new_acting_name = param @Text "acting_for"
        setSuccessMessage $ "You are now acting for " <> new_acting_name
        setSession "acting_for" new_acting_name
        redirectTo RegistrationsAction

    action StopActingAction = do
        deleteSession "acting_for"
        redirectTo RegistrationsAction

    action DeleteSessionAction = do
        deleteSession "name"
        redirectTo RegistrationsAction
