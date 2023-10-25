module Web.Routes where
import IHP.RouterPrelude
import Generated.Types
import Web.Types

instance CanRoute RegistrationsController where
  parseRoute' = choice
    [ CalendarAction Nothing <$ string "/calendar.ics" <* endOfInput
    , CalendarAction . Just <$> (string "/calendar-" *> parseId <* string ".ics" <* endOfInput)
    , autoRoute
    ]

-- Generator Marker
instance AutoRoute StaticController
instance AutoRoute RegistrationsController
instance AutoRoute LogsController
instance AutoRoute SessionsController
instance AutoRoute KeyHoldersController
instance AutoRoute (Id' "users")

