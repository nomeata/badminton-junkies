module Web.Controller.Registrations where

import Web.Controller.Prelude
import Web.View.Registrations.Index

import Data.Time.Zones
import Control.Monad.Trans.Except

instance Controller RegistrationsController where
    action RegistrationsAction = autoRefresh do
        now <- getCurrentTime

        signed_up_for <- findSignUp

        upcoming_dates <- upcomingDates >>= mapM (\pd -> do
            let d = get #pd_date pd
            regs <- query @Registration
               |> filterWhere (#date, d)
               |> orderBy #createdAt
               |> fetch
               >>= collectionFetchRelatedOrNothing #playerUser
            pure (pd, now < pd_date pd, regs)
         )
        render IndexView { .. }

    action RegisterAction = do
        needAuth

        let date = param @UTCTime "date"
        sd <- fromMaybe (error "should not happen") <$> currentSD
        let name = actingName sd

        isPlayingDateOpen date
        isRegistered name >>= \case
            Left d | date == d ->
                err $ name <> " is already registered on this day"
                   | otherwise ->
                err $ name <> " is already registered on another day"
            Right () -> pure ()

        pos <- withTransaction $ do
            hasKey <- query @Keyholder
                |> filterWhere (#holder, name)
                |> fetchExists
            date' <- prettyTime date
            logMessage [trimming|registered ${name} for ${date'}|]
            reg <- newRecord @Registration
                |> set #playerName name
                |> set #playerUser (if isJust (actingFor sd) then Nothing else Just (user sd |> get #id))
                |> set #date date
                |> set #hasKey hasKey
                |> createRecord

            getPos reg

        if isWaitlist pos
          then err $ "You have put " <> name <> " on the waitlist."
          else ok $ "You have registered " <> name <> "."

    action DeleteRegistrationAction { registrationId } = do
        needAuth

        reg <- fetch registrationId
        pos <- getPos reg
        unless (isWaitlist pos) $ do
            isPlayingDateOpen (get #date reg)

        withTransaction $ do
            let name = get #playerName reg
            date <- prettyTime $ get #date reg
            logMessage [trimming|removed registration of ${name} for ${date}|]
            deleteRecord reg
        ok $ "You have unregistered " <> get #playerName reg <> "."

    action SetKeyRegistrationAction { registrationId } = do
        needAuth

        let hasKey = param @Bool "hasKey"
        withTransaction $ do
            reg <- fetch registrationId
            when (get #hasKey reg == hasKey) $
                err $ "Nothing to do"
            let name = get #playerName reg
            date <- prettyTime $ get #date reg
            logMessage $
                if hasKey
                then [trimming|notes that ${name} has a key on ${date}|]
                else [trimming|notes that ${name} has no key on ${date}|]
            reg |> set #hasKey hasKey |> updateRecord
        ok "Noted!"


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

needAuth :: (?context::ControllerContext) => IO ()
needAuth = fromContext @(Maybe SessionData) >>= \case
    Just sd -> pure ()
    Nothing -> err "Please log in first"

currentSD :: (?context::ControllerContext) => IO (Maybe SessionData)
currentSD = fromContext @(Maybe SessionData)

actingName :: SessionData -> Text
actingName sd = fromMaybe (userName (user sd)) (actingFor sd)

-- Assumes that the name is signed up
getPos :: (?modelContext::ModelContext) => Registration -> IO Int
getPos reg = do
    regs <- query @Registration
       |> filterWhere (#date, get #date reg)
       |> orderBy #createdAt
       |> fetch
    let Just pos = succ <$> findIndex (\r -> r == reg) regs
    return pos


findSignUp :: (?modelContext::ModelContext, ?context::ControllerContext) =>
    IO (Maybe (Registration, PlayDate, Bool))
findSignUp = do
    currentSD >>= \case
        Nothing -> pure Nothing
        Just sd -> do
            let name = actingName sd
            pds <- upcomingDates
            now <- getCurrentTime
            fmap (either Just (const Nothing)) $ runExceptT $ forM_ pds $ \pd ->
              when (now < get #pd_reg_block_over pd) $ do
                regs <- liftIO $
                  query @Registration
                  |> filterWhere (#date, get #pd_date pd)
                  |> filterWhere (#playerName, name)
                  |> fetchOneOrNothing

                forEach regs $ \reg -> do
                    pos <- liftIO $ getPos reg
                    let open = now < pd_date pd
                    let can_unregister = open || isWaitlist pos
                    throwE (reg, pd, can_unregister)


isRegistered :: (?modelContext::ModelContext) => Text -> IO (Either UTCTime ())
isRegistered name = do
    pds <- upcomingDates
    now <- getCurrentTime
    runExceptT $ forM_ pds $ \pd ->
      when (now < get #pd_reg_block_over pd) $ do
        regs <- liftIO $
          query @Registration
          |> filterWhere (#date, get #pd_date pd)
          |> filterWhere (#playerName, name)
          |> fetch
        unless (null regs) $ throwE (get #pd_date pd)

prettyTime :: UTCTime -> IO Text
prettyTime t = do
    tz <- loadTZFromDB "Europe/Berlin"
    pure $ t |> utcToLocalTimeTZ tz |> formatTime defaultTimeLocale "%d.%m.%Y, %H:%M" |> cs

