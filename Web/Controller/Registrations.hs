module Web.Controller.Registrations where

import Web.Controller.Prelude
import Web.View.Registrations.Index

import Data.Time.Zones

instance Controller RegistrationsController where
    action RegistrationsAction = autoRefresh do
        upcoming_dates <- upcomingDates >>= mapM (\pd -> do
            let d = get #pd_date pd
            regs <- query @Registration
               |> filterWhere (#date, d)
               |> fetch
            let formreg = (newRecord @Registration) { date = d }
            pure (pd, regs, formreg)
         )
        render IndexView { .. }

    action CreateRegistrationAction = do
        let reg = newRecord @Registration
              |> fill @["playerName","date"]

        when (null (get #playerName reg)) $
            err "No player name given."

        now <- getCurrentTime
        pds <- upcomingDates
        case find (\pd -> get #date reg == pd_date pd) pds of
          Nothing -> err "This playing date is not up for registration"
          Just pd -> do
            unless (now < pd_date pd) $
              err "This playing date has already started"
            unless (pd_reg_opens pd <= now) $
              err "This playing date is not yet open for registration"

            forM_ pds $ \pd ->
              when (now < get #pd_reg_block_over pd) $ do
                regs <- query @Registration
                 |> filterWhere (#date, get #pd_date pd)
                 |> filterWhere (#playerName, get #playerName reg)
                 |> fetch
                unless (null regs) $
                  err $ get #playerName reg <> " is already registered on another day"

            reg |> createRecord
            ok $ "You have registered " <> get #playerName reg <> "."

    action DeleteRegistrationAction { registrationId } = do
        reg <- fetch registrationId
        deleteRecord reg
        ok $ "You have unregistered " <> get #playerName reg <> "."

err msg = setErrorMessage msg >> redirectTo RegistrationsAction
ok msg = setSuccessMessage msg >> redirectTo RegistrationsAction

upcomingDates :: IO [PlayDate]
upcomingDates = do
    now <- getCurrentTime
    tz <- loadTZFromDB "Europe/Berlin"
    let LocalTime today _ = utcToLocalTimeTZ tz now
    pure $
     [ PlayDate
       { pd_date = localTimeToUTCTZ tz (LocalTime day time)
       , pd_reg_opens = localTimeToUTCTZ tz (LocalTime (reg_days_diff `addDays` day) reg_time)
       , pd_reg_block_over = localTimeToUTCTZ tz (LocalTime day (TimeOfDay 20 30 00))
       }
     | day <- [today .. 6 `addDays` today]
     , (time, reg_days_diff, reg_time) <- case dayOfWeek day of
        Tuesday -> [ (TimeOfDay 17 00 00, -6, TimeOfDay 10 30 00) ]
        Sunday ->  [ (TimeOfDay 14 00 00, -6, TimeOfDay 10 30 00)
                   , (TimeOfDay 17 00 00, -6, TimeOfDay 10 30 00) ]
        _      ->  []
     ]


