module Web.Controller.Registrations where

import Web.Controller.Prelude
import Web.View.Registrations.Index
import Web.View.Registrations.New
import Web.View.Registrations.Edit
import Web.View.Registrations.Show

import Data.Time.Zones

instance Controller RegistrationsController where
    action RegistrationsAction = do
        upcoming_dates <- upcomingDates >>= mapM (\d -> do
            regs <- query @Registration |> filterWhere (#date, d) |> fetch
            let formreg = (newRecord @Registration) { date = d }
            pure (d, regs, formreg)
         )
        render IndexView { .. }

    action NewRegistrationAction = do
        let registration = newRecord
        render NewView { .. }

    action ShowRegistrationAction { registrationId } = do
        registration <- fetch registrationId
        render ShowView { .. }

    action EditRegistrationAction { registrationId } = do
        registration <- fetch registrationId
        render EditView { .. }

    action UpdateRegistrationAction { registrationId } = do
        registration <- fetch registrationId
        registration
            |> buildRegistration
            |> ifValid \case
                Left registration -> render EditView { .. }
                Right registration -> do
                    registration <- registration |> updateRecord
                    setSuccessMessage "Registration updated"
                    redirectTo EditRegistrationAction { .. }

    action CreateRegistrationAction = do
        let registration = newRecord @Registration
        registration
            |> buildRegistration
            |> ifValid \case
                Left registration -> do
                    setErrorMessage "Could not add registration"
                    redirectTo RegistrationsAction
                Right registration -> do
                    registration <- registration |> createRecord
                    setSuccessMessage "Registration created"
                    redirectTo RegistrationsAction

    action DeleteRegistrationAction { registrationId } = do
        registration <- fetch registrationId
        deleteRecord registration
        setSuccessMessage "Registration deleted"
        redirectTo RegistrationsAction

buildRegistration registration = registration
    |> fill @["playerName","date"]
    |> validateField #playerName nonEmpty

upcomingDates :: IO [UTCTime]
upcomingDates = do
    now <- getCurrentTime
    tz <- loadTZFromDB "Europe/Berlin"
    let LocalTime today _ = utcToLocalTimeTZ tz now
    pure $
     [ localTimeToUTCTZ tz (LocalTime day time)
     | day <- [today .. 6 `addDays` today]
     , time <- case dayOfWeek day of
        Tuesday -> [ TimeOfDay 17 00 00 ]
        Sunday ->  [ TimeOfDay 14 00 00, TimeOfDay 17 00 00 ]
        _      ->  []
     ]
