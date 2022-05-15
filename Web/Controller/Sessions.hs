module Web.Controller.Sessions where

import Application.BuhlAuth

import Web.Controller.Prelude
import Web.View.Sessions.New

instance Controller SessionsController where
    action NewSessionAction = do
        let login_data = newRecord @LoginData
        render LoginView { .. }

    action CreateSessionAction = do
        let login_data = newRecord @LoginData
              |> fill @["email","password"]
              |> validateField #email isEmail

        flip ifValid login_data $ \case
            Left login_data ->
                render LoginView { .. }
            Right login_data -> do
                buhlLogin (get #email login_data) (get #password login_data) >>= \case
                    Left msg -> do
                        setErrorMessage $"Login failed: " <> msg
                        render LoginView { .. }
                    Right name -> do
                        setSuccessMessage $ "Welcome, " <> name
                        setSession "name" name
                        redirectTo RegistrationsAction

    action DeleteSessionAction = do
        deleteSession "name"
        redirectTo RegistrationsAction
