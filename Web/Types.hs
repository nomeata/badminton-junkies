module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)

data StaticController = WelcomeAction deriving (Eq, Show, Data)

data RegistrationsController
    = RegistrationsAction
    | TrialsAction
    | PastAction { page :: Maybe Integer }
    | RegisterAction { fromTrial :: !Text }
    | DeleteRegistrationAction { fromTrial :: !Text, registrationId :: !(Id Registration) }
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

data KeyHoldersController
    = KeyHoldersAction
    | ChangeKeyHolder
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
    user :: User,
    actingFor :: Maybe User
    }


userName :: User -> Text
userName u = fromMaybe (fullname u) (nickname u)

type Reg = Registration' (Maybe User)

regName :: Reg -> Text
regName reg | Just user <- get #playerUser reg = userName user
            | Just trialUser <- get #playerName reg = trialUser
            | otherwise = "?"

regIsTrial :: Reg -> Bool
regIsTrial reg = isNothing reg.playerUser


data PlayDate = PlayDate
  { pd_date            :: UTCTime
  , pd_reg_opens       :: UTCTime
  , pd_trial_reg_opens :: UTCTime
  , pd_reg_block_over  :: UTCTime
  }

