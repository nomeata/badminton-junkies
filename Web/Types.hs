module Web.Types where

import IHP.Prelude
import IHP.ModelSupport
import Generated.Types

data WebApplication = WebApplication deriving (Eq, Show)


data StaticController = WelcomeAction deriving (Eq, Show, Data)

data RegistrationsController
    = RegistrationsAction
    | NewRegistrationAction
    | ShowRegistrationAction { registrationId :: !(Id Registration) }
    | CreateRegistrationAction
    | EditRegistrationAction { registrationId :: !(Id Registration) }
    | UpdateRegistrationAction { registrationId :: !(Id Registration) }
    | DeleteRegistrationAction { registrationId :: !(Id Registration) }
    deriving (Eq, Show, Data)
