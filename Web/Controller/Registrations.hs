module Web.Controller.Registrations where

import Web.Controller.Prelude
import Web.View.Registrations.Index
import Web.View.Registrations.Past
import Web.View.Registrations.Trials
import Web.View.Registrations.Stats
import Web.View.Registrations.Calendar

import Network.Wai (responseLBS)
import Network.HTTP.Types (status200, status404)
import Network.HTTP.Types.Header

import Data.Time.Zones
import Control.Monad.Trans.Except

instance Controller RegistrationsController where
    action RegistrationsAction = autoRefresh do
        now <- getCurrentTime

        signed_up_for <- findSignUp

        keyholders <- map (.userId) <$> query @Keyholder |> fetch
        let has_key Nothing = False
            has_key (Just user) = user.id `elem` keyholders

        upcoming_dates <- upcomingDates >>= mapM (\pd -> do
            let d = get #pd_date pd
            regs <- query @Registration
               |> filterWhere (#date, d)
               |> orderBy #createdAt
               |> fetch
               >>= collectionFetchRelatedOrNothing #playerUser
               >>= pure .  map (\r -> (r, has_key (r.playerUser)))

            pure (pd, isRegOpen "main" now pd, regs)
         )


        render IndexView { .. }

    action TrialsAction = autoRefresh do
        now <- getCurrentTime

        upcoming_dates <- upcomingDates >>= mapM (\pd -> do
            let d = get #pd_date pd
            regs <- query @Registration
               |> filterWhere (#date, d)
               |> orderBy #createdAt
               |> fetch
               >>= collectionFetchRelatedOrNothing #playerUser
            pure (pd, isRegOpen "trials" now pd, regs)
         )
        render TrialView { .. }

    action PastAction {..} = autoRefresh do
        (dateQ, pagination) <- query @Registration
          |> filterWherePast #date
          |> orderByDesc #date
          |> distinctOn #date
          |> paginate

        dates <- dateQ |> fetch >>= mapM (\r -> do
            let d = r.date
            regs <- query @Registration
               |> filterWhere (#date, d)
               |> orderBy #createdAt
               |> fetch
               >>= collectionFetchRelatedOrNothing #playerUser
            pure (d, regs)
         )

        render PastView {..}

    action StatsAction = autoRefresh do
        needAuth "main"
        trackTableRead "registrations"
        regCount' :: [(Maybe (Id User), Maybe Text, Int)] <- sqlQuery
            "SELECT player_user, player_name, count(*) AS n \
            \FROM (\
              \SELECT player_user, player_name, \
              \ RANK () OVER (PARTITION BY date ORDER BY created_at) AS pos \
              \ FROM registrations \
              \ WHERE date > NOW() - interval '3 months' \
            \  ) AS ranks \
            \WHERE pos <= 9 \
            \GROUP BY player_user, player_name \
            \ORDER BY n DESC" ()
        regCount <- forM regCount' \case
            (Nothing, Just n, c) -> pure $ (Right n, c)
            (Just id, _, c) -> do
                user <- fetch id
                pure (Left user, c)
            (Nothing, Nothing, c) -> pure $ (Right "?", c)

        render StatsView {..}

    action (CalendarAction mbuid) = do
        now <- getCurrentTime

        forM_ mbuid $ \uid ->
            fetchOneOrNothing uid >>= \case
                Just _ -> pure ()
                Nothing -> respondAndExit $
                    responseLBS status404 [(hContentType, "text/plain")]
                        "Cannot produce calendar, user unknown"

        upcoming_dates <- upcomingDates >>= mapM (\pd -> do
            let d = get #pd_date pd
            regs <- query @Registration
               |> filterWhere (#date, d)
               |> orderBy #createdAt
               |> fetch
               >>= collectionFetchRelatedOrNothing #playerUser
            pure (pd, regs)
            )

        upcoming_dates <- return $ case mbuid of
            Just uid -> filter (\(_, regs) -> any (\r -> case r.playerUser of
                Just u -> u.id == uid
                Nothing -> False) regs) upcoming_dates
            Nothing -> upcoming_dates

        respondAndExit $ responseLBS status200 [(hContentType, "text/calendar; charset=UTF-8")] $
           renderCalendar CalendarView { .. }

    action RegisterAction { fromTrial } = do
        needAuth fromTrial

        date <- case paramOrNothing @UTCTime "date" of
            Just date -> pure date
            Nothing -> err fromTrial $ "Please select one of the playing dates"
        sd <- fromMaybe (error "should not happen") <$> currentSD

        isPlayingDateOpen fromTrial date

        case paramOrNothing @Text "trialname" of
          Just trialName -> do
            isRegisteredTrial trialName >>= \case
                Left d | date == d ->
                    err fromTrial $ trialName <> " is already registered on this day"
                       | otherwise ->
                    err fromTrial $ trialName <> " is already registered on another day"
                Right () -> pure ()

            pos <- withTransaction $ do
                date' <- prettyTime date
                logMessage [trimming|registered ${trialName} for ${date'}|]
                reg <- newRecord @Registration
                    |> set #playerName (Just trialName)
                    |> set #playerUser Nothing
                    |> set #date date
                    |> createRecord

                getPos reg

            if isWaitlist pos
              then err fromTrial $ "You have put " <> trialName <> " on the waitlist."
              else ok fromTrial $ "You have registered " <> trialName <> "."
          Nothing -> do
            let name = userName (actingUser sd)
            isRegisteredUser (actingUser sd) >>= \case
                Left d | date == d ->
                    err fromTrial $ name <> " is already registered on this day"
                       | otherwise ->
                    err fromTrial $ name <> " is already registered on another day"
                Right () -> pure ()

            pos <- withTransaction $ do
                let name = userName (actingUser sd)
                date' <- prettyTime date
                logMessage [trimming|registered ${name} for ${date'}|]
                reg <- newRecord @Registration
                    |> set #playerName Nothing
                    |> set #playerUser (Just ((actingUser sd).id))
                    |> set #date date
                    |> createRecord

                getPos reg

            if isWaitlist pos
              then err fromTrial $ "You have put " <> name <> " on the waitlist."
              else ok fromTrial $ "You have registered " <> name <> "."

    action DeleteRegistrationAction { fromTrial, registrationId } = do
        needAuth fromTrial

        reg  :: Registration <- fetch registrationId
        reg' :: Reg <- reg |> fetchRelatedOrNothing #playerUser

        pos <- getPos reg
        unless (isWaitlist pos) $ do
            isPlayingDateOpen fromTrial (get #date reg)

        withTransaction $ do
            date <- prettyTime $ get #date reg
            let name = regName reg'
            logMessage [trimming|removed registration of ${name} for ${date}|]
            deleteRecord reg
        ok fromTrial $ "You have unregistered " <> regName reg' <> "."

isRegOpen fromTrial now pd
    | fromTrial == "trials" && now < pd_trial_reg_opens pd
    = NotYet  (pd_trial_reg_opens pd)
    | now < pd_reg_opens pd
    = NotYet  (pd_reg_opens pd)
    | pd_date pd < now
    = Closed
    | otherwise
    = Open

isPlayingDateOpen fromTrial d = do
    now <- getCurrentTime
    pds <- upcomingDates
    case find (\pd -> d == pd_date pd) pds of
      Nothing -> err fromTrial "This playing date is not up for registration"
      Just pd -> case isRegOpen fromTrial now pd of
        NotYet date -> do
          date' <- prettyTime date
          err fromTrial $ "This playing date is not yet open for registration. It opens in " <> date'
        Closed -> err fromTrial "This playing date has already started"
        Open -> pure ()

-- Query parameters cannot be Bool :-(
ok, err :: (?context::ControllerContext) => Text -> Text -> IO a
err "main" msg = setErrorMessage msg >> redirectTo RegistrationsAction >> pure undefined
err "trials" msg = setErrorMessage msg >> redirectTo TrialsAction >> pure undefined
err x _ = error $ "Unexpected value " <> show x
ok "main" msg = setSuccessMessage msg >> redirectTo RegistrationsAction >> pure undefined
ok "trials" msg = setSuccessMessage msg >> redirectTo TrialsAction >> pure undefined
ok x _ = error $ "Unexpected value " <> show x

upcomingDates :: IO [PlayDate]
upcomingDates = do
    now <- getCurrentTime
    tz <- loadTZFromDB "Europe/Berlin"
    let LocalTime today _ = utcToLocalTimeTZ tz now
    pure $
     [ PlayDate
       { pd_date =
            localTimeToUTCTZ tz (LocalTime day time)
       , pd_until =
            localTimeToUTCTZ tz ((3600*hours) `addLocalTime` (LocalTime day time))
       , pd_reg_opens =
            localTimeToUTCTZ tz (LocalTime (reg_days_diff `addDays` day) reg_time)
       , pd_trial_reg_opens =
            localTimeToUTCTZ tz (LocalTime (trial_reg_days_diff `addDays` day) trial_reg_time)
       , pd_reg_block_over =
            localTimeToUTCTZ tz (LocalTime day (TimeOfDay 20 30 00))
       }
     | day <- [today .. 6 `addDays` today]
     , (time, reg_days_diff, reg_time, trial_reg_days_diff, trial_reg_time, hours) <-
        case dayOfWeek day of
            Tuesday -> [ (TimeOfDay 17 00 00, -6, TimeOfDay 20 30 00, -1, TimeOfDay 17 00 00, 2) ]
            Sunday ->  [ (TimeOfDay 14 00 00, -6, TimeOfDay 20 30 00, -1, TimeOfDay 14 00 00, 3)
                       , (TimeOfDay 17 00 00, -6, TimeOfDay 20 30 00, -1, TimeOfDay 17 00 00, 3) ]
            _      ->  []
     ]

needAuth :: (?context::ControllerContext) => Text -> IO ()
needAuth fromTrial = fromContext @(Maybe SessionData) >>= \case
    Just sd -> pure ()
    Nothing -> err fromTrial "Please log in first"

currentSD :: (?context::ControllerContext) => IO (Maybe SessionData)
currentSD = fromContext @(Maybe SessionData)

actingUser :: SessionData -> User
actingUser sd = fromMaybe (user sd) (actingFor sd)

actingName :: SessionData -> Text
actingName sd = userName (actingUser sd)

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
            pds <- upcomingDates
            now <- getCurrentTime
            fmap (either Just (const Nothing)) $ runExceptT $ forM_ pds $ \pd ->
              when (now < get #pd_reg_block_over pd) $ do
                regs <- liftIO $
                  query @Registration
                  |> filterWhere (#date, get #pd_date pd)
                  |> filterWhere (#playerUser, Just ((actingUser sd).id))
                  |> fetchOneOrNothing

                forEach regs $ \reg -> do
                    pos <- liftIO $ getPos reg
                    let open = now < pd_date pd
                    let can_unregister = open || isWaitlist pos
                    throwE (reg, pd, can_unregister)


isRegisteredTrial :: (?modelContext::ModelContext) => Text -> IO (Either UTCTime ())
isRegisteredTrial name = do
    pds <- upcomingDates
    now <- getCurrentTime
    runExceptT $ forM_ pds $ \pd ->
      when (now < get #pd_reg_block_over pd) $ do
        regs <- liftIO $
          query @Registration
          |> filterWhere (#date, get #pd_date pd)
          |> filterWhere (#playerName, Just name)
          |> fetch
        unless (null regs) $ throwE (get #pd_date pd)

isRegisteredUser :: (?modelContext::ModelContext) => User -> IO (Either UTCTime ())
isRegisteredUser user = do
    pds <- upcomingDates
    now <- getCurrentTime
    runExceptT $ forM_ pds $ \pd ->
      when (now < get #pd_reg_block_over pd) $ do
        regs <- liftIO $
          query @Registration
          |> filterWhere (#date, get #pd_date pd)
          |> filterWhere (#playerUser, Just user.id)
          |> fetch
        unless (null regs) $ throwE (get #pd_date pd)

prettyTime :: UTCTime -> IO Text
prettyTime t = do
    tz <- loadTZFromDB "Europe/Berlin"
    pure $ t |> utcToLocalTimeTZ tz |> formatTime defaultTimeLocale "%d.%m.%Y, %H:%M" |> cs

