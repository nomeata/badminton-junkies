module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

instance CanRoute RegistrationsController where
  parseRoute' = choice
    [ string "/calendar.ics" <* endOfInput >> pure CalendarAction
    , autoRoute
    ]

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute RegistrationsController
instance AutoRoute LogsController
instance AutoRoute SessionsController
instance AutoRoute KeyHoldersController
instance AutoRoute (Id' "users")

