module Web.Controller.Sessions where

import Application.BuhlAuth

import Web.Controller.Prelude
import Web.View.Sessions.Edit
import Web.View.Sessions.Login

instance Controller SessionsController where
    action EditSessionAction = fromContext @(Maybe SessionData) >>= \case
        Just sessionData -> do
            -- User is logged in
            other_users <- query @User
                |> orderByAsc #fullname
                |> fetch
            other_users :: [User] <- sqlQuery
                "SELECT * FROM users \
                \WHERE id IN (\
                  \SELECT player_user \
                  \ FROM registrations \
                  \ WHERE date > NOW() - interval '3 months' \
                \  ) \
                \AND id != ? \
                \ORDER BY fullname DESC" (Only sessionData.user.id)
            render EditView { .. }
        Nothing -> do
            -- User is not logged in
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
                        setErrorMessage $ "Login failed: " <> msg
                        render LoginView { .. }
                    Right (BuhlAccountData {buhlId, firstName, lastName, email, clubs}) -> do
                        unless (badmintonJunkiesId `elem` clubs) $ do
                            setErrorMessage "It looks like you are not a member yet."
                            render LoginView { .. }

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
