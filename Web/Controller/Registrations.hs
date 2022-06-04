module Web.Controller.Registrations where

import Web.Controller.Prelude
import Web.View.Registrations.Index

import Data.Time.Zones

instance Controller RegistrationsController where
    action RegistrationsAction = autoRefresh do
        now <- getCurrentTime
        upcoming_dates <- upcomingDates >>= mapM (\pd -> do
            let d = get #pd_date pd
            regs <- query @Registration
               |> filterWhere (#date, d)
               |> orderBy #createdAt
               |> fetch

            if now < pd_date pd
            then do
                formreg <- fmap (\n -> (newRecord @Registration) { date = d, playerName = n} )
                    <$> getSession "name"
                let reg_lines = (map R regs ++ [F formreg]) |> fillUp 9 E
                pure (pd, True, reg_lines)
            else do
                let reg_lines = map R regs |> fillUp 9 E
                pure (pd, False, reg_lines)
         )
        render IndexView { .. }

    action CreateRegistrationAction = do
        authname <- getSession "name" >>= \case
            Nothing -> err "Please log in first"
            Just n -> pure (n :: Text)

        let reg = newRecord @Registration
              |> fill @["playerName","date"]

        when (null (get #playerName reg)) $
            err "No player name given."

        isPlayingDateOpen (get #date reg)

        pds <- upcomingDates
        now <- getCurrentTime
        forM_ pds $ \pd ->
          when (now < get #pd_reg_block_over pd) $ do
            regs <- query @Registration
             |> filterWhere (#date, get #pd_date pd)
             |> filterWhere (#playerName, get #playerName reg)
             |> fetch
            unless (null regs) $
                if get #date reg == get #pd_date pd
                then err $ get #playerName reg <> " is already registered on this day"
                else err $ get #playerName reg <> " is already registered on another day"

        withTransaction $ do
            let name = get #playerName reg
            date <- prettyTime $ get #date reg
            logMessage authname [trimming|registered ${name} for ${date}|]
            reg |> createRecord
        ok $ "You have registered " <> get #playerName reg <> "."

    action DeleteRegistrationAction { registrationId } = do
        authname <- getSession "name" >>= \case
            Nothing -> err "Please log in first"
            Just n -> pure (n :: Text)

        reg <- fetch registrationId

        isPlayingDateOpen (get #date reg)

        withTransaction $ do
            let name = get #playerName reg
            date <- prettyTime $ get #date reg
            logMessage authname [trimming|removed registration of ${name} for ${date}|]
            deleteRecord reg
        ok $ "You have unregistered " <> get #playerName reg <> "."

    action SetKeyRegistrationAction { registrationId } = do
        authname <- getSession "name" >>= \case
            Nothing -> err "Please log in first"
            Just n -> pure (n :: Text)

        let hasKey = param @Bool "hasKey"
        withTransaction $ do
            reg <- fetch registrationId
            when (get #hasKey reg == hasKey) $
                err $ "Nothing to do"
            let name = get #playerName reg
            date <- prettyTime $ get #date reg
            logMessage authname $
                if hasKey
                then [trimming|notes that ${name} has a key on ${date}|]
                else [trimming|notes that ${name} has no key on ${date}|]
            reg |> set #hasKey hasKey |> updateRecord
        ok "Noted!"


logMessage authname txt = do
    newRecord @Log |> set #text (authname <> " " <> txt) |> createRecord

isPlayingDateOpen d = do
    now <- getCurrentTime
    pds <- upcomingDates
    case find (\pd -> d == pd_date pd) pds of
      Nothing -> err "This playing date is not up for registration"
      Just pd -> do
        unless (now < pd_date pd) $
          err "This playing date has already started"
        unless (pd_reg_opens pd <= now) $
          err "This playing date is not yet open for registration"

err msg = setErrorMessage msg >> redirectTo RegistrationsAction >> pure undefined
ok msg = setSuccessMessage msg >> redirectTo RegistrationsAction >> pure undefined

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
        Tuesday -> [ (TimeOfDay 17 00 00, -6, TimeOfDay 20 30 00) ]
        Sunday ->  [ (TimeOfDay 14 00 00, -6, TimeOfDay 20 30 00)
                   , (TimeOfDay 17 00 00, -6, TimeOfDay 20 30 00) ]
        _      ->  []
     ]

prettyTime :: UTCTime -> IO Text
prettyTime t = do
    tz <- loadTZFromDB "Europe/Berlin"
    pure $ t |> utcToLocalTimeTZ tz |> formatTime defaultTimeLocale "%d.%m.%Y, %H:%M" |> cs

