module Web.Controller.Sessions where

import Application.BuhlAuth

import Web.Controller.Prelude
import Web.View.Sessions.Edit

instance Controller SessionsController where
    action EditSessionAction = do
        other_users <- query @User |> fetch

        let login_data = newRecord @LoginData
        render EditView { .. }

    action CreateSessionAction = do
        other_users <- query @User |> fetch

        let login_data = newRecord @LoginData
              |> fill @["email","password"]
              |> validateField #email isEmail

        flip ifValid login_data $ \case
            Left login_data ->
                render EditView { .. }
            Right login_data -> do
                buhlLogin (get #email login_data) (get #password login_data) >>= \case
                    Left msg -> do
                        setErrorMessage $ "Login failed: " <> msg
                        render EditView { .. }
                    Right (BuhlAccountData {buhlId, firstName, lastName, email, clubs}) -> do
                        unless (badmintonJunkiesId `elem` clubs) $ do
                            setErrorMessage "It looks like you are not a member yet."
                            render EditView { .. }

                        let fullName = firstName <> " " <> lastName

                        user <- withTransaction $ do
                            mbuser <- query @User |> filterWhere (#buhlId, buhlId) |> fetchOneOrNothing
                            case mbuser of
                                Just user' -> do
                                    now <- getCurrentTime
                                    -- seen this user before, just update with potentially new data
                                    user' |> set #fullname fullName
                                          |> set #email (Just email)
                                          |> set #lastLogin now
                                          |> updateRecord
                                Nothing -> do
                                    -- Transition period: Try to match by name until the new buhlid is used everywhere
                                    mbuser <- query @User |> filterWhere (#fullname, fullName) |> fetchOneOrNothing
                                    case mbuser of
                                        Just user' -> do
                                            now <- getCurrentTime
                                            -- Store new buhlid
                                            user' |> set #buhlId buhlId
                                                  |> set #email (Just email)
                                                  |> set #lastLogin now
                                                  |> updateRecord
                                        Nothing -> do
                                            -- new user
                                            newRecord @User
                                                |> set #fullname fullName
                                                |> set #buhlId buhlId
                                                |> set #email (Just email)
                                                |> createRecord

                        setSuccessMessage $ "Welcome, " <> fullName
                        setSession "userid" (user |> get #id)
                        redirectTo RegistrationsAction

    action ChangeNameAction = do
        sd <- needAuth
        if hasParam "clear" then do
            user sd |> set #nickname Nothing |> updateRecord
            setSuccessMessage $ "You are now known as " <> fullname (user sd)
        else do
            let new_name = param @Text "nickname"
            user sd |> set #nickname (Just new_name) |> updateRecord
            setSuccessMessage $ "You are now known as " <> new_name
        redirectTo RegistrationsAction

    action ChangeActingAction = do
        let other_user_id = param @(Id User) "acting_for"
        other_user <- fetch other_user_id

        setSuccessMessage $ "You are now acting for " <> userName other_user
        setSession "acting_for_id" other_user_id
        redirectTo RegistrationsAction

    action StopActingAction = do
        deleteSession "acting_for_id"
        redirectTo RegistrationsAction

    action DeleteSessionAction = do
        deleteSession "userid"
        redirectTo RegistrationsAction

needAuth :: (?context::ControllerContext) => IO SessionData
needAuth = fromContext @(Maybe SessionData) >>= \case
    Just sd -> pure sd
    Nothing -> err "Please log in first"

err msg = setErrorMessage msg >> redirectTo EditSessionAction >> pure undefined
