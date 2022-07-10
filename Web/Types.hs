module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

data RegistrationsController
    = RegistrationsAction
    | CreateRegistrationAction
    | DeleteRegistrationAction { registrationId :: !(Id Registration) }
    | SetKeyRegistrationAction { registrationId :: !(Id Registration) }
    deriving (Eq, Show, Data)

newtype LogsController
    = LogsAction { page :: Maybe Integer }
    deriving (Eq, Show, Data)

data SessionsController
    = CreateSessionAction
    | DeleteSessionAction
    | EditSessionAction
    | ChangeNameAction
    | ChangeActingAction
    | StopActingAction
    deriving (Eq, Show, Data)

data LoginData = LoginData { email :: Text, password :: Text, meta :: MetaBag}
    deriving (Eq, Show)
instance Record LoginData where
    {-# INLINE newRecord #-}
    newRecord = LoginData def def def
instance SetField "email" LoginData Text where
    {-# INLINE setField #-}
    setField newValue (LoginData email password meta) =
        LoginData newValue password (meta { touchedFields = "email" : touchedFields meta })
instance SetField "password" LoginData Text where
    {-# INLINE setField #-}
    setField newValue (LoginData email password meta) =
        LoginData email newValue (meta { touchedFields = "password" : touchedFields meta })
instance SetField "meta" LoginData MetaBag where
    {-# INLINE setField #-}
    setField newValue (LoginData email password meta) =
        LoginData email password newValue


data SessionData = SessionData {
    fullName :: Text,
    nickname :: Text,
    actingFor :: Maybe Text
    }

