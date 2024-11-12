module Web.View.Registrations.Index where
import Web.View.Prelude
import IHP.View.TimeAgo
import Data.Time.Clock.POSIX

type RegWithKey = (Reg, Bool)

data IndexView = IndexView
  { signed_up_for :: Maybe (Registration, PlayDate, Bool)
  , upcoming_dates :: [(PlayDate, RegOpenness, [RegWithKey])]
  }

data Entry = R (RegWithKey) | E

instance View IndexView where
    html IndexView { .. } = [hsx|
      {signUpRow signed_up_for upcoming_dates}
      <div class="row">
       {forEach upcoming_dates renderUpcomingDate}
      </div>
      <div class="row">
       <div class="col-lg-4">
        <div class="card mb-4 box-shadow md-4">
        <div class="card-header">
        <h2>Instructions</h2>
        </div>
        <div class="card-body">
        <p>Once you have logged in, you can sign up with the big buttons on top. The first 9 members to register get to play, if you add yourself later you are on the waitlist.</p>
        <p>Sign-up for the next playing day opens at 20:30. You can sign up for one play time at a time. If you have just played, wait until 22:30 before signing up for the next play time.</p>
        <p>You can unregister. People on the waitlist will then move up. Please also message the group to tell them that a spot is now open.</p>
        <p>To change your nickname or register other members, click the 🛠 button in the top-right corner. To sign up trials (“Schnupperer”), use the 👃 page.</p>
        <p>The system keeps a log of registrations and removals.</p>
        </div>
        </div>
       </div>

       <div class="col-lg-4">
        <div class="card mb-4 box-shadow md-4">
        <div class="card-header">
        <h2>Calendar</h2>
        </div>
        <div class="card-body">
        <p>You can import the upcoming dates into your calendar app, using the following link (in iCalender format):</p>
        <pre style="overflow-x:scroll; font-family:monospace"><a href="/calendar.ics">https://badjunk.nomeata.de/calendar.ics</a><br/>&nbsp;</pre>
        {userCalendar}
        </div>
        </div>
       </div>

       <div class="col-lg-4">
        <div class="card mb-4 box-shadow md-4">
        <div class="card-header">
        <h2>Contact</h2>
        </div>
        <div class="card-body">
        <p>This website was built by Joachim Breitner for the Badminton Junkies. In case of problems and questions, <a href="mailto:mail@joachim-breitner.de">send me an e-mail</a> or talk to me on WhatsApp.</p>
        <p>You can <a href="https://github.com/nomeata/badminton-junkies">inspect the source code</a> and make contributions there!</p>
        </div>
        </div>
       </div>
      </div>
    |]
      where
        userCalendar = case fromFrozenContext :: Maybe SessionData of
            Just (SessionData {user = user}) -> [hsx|
              <p>A calendar with just the events that <em>you</em> have signed up for can be found here:</p>
              <pre style="overflow-x:scroll; font-family:monospace"><a href={"/calendar-" <> show (user.id) <> ".ics"}>https://badjunk.nomeata.de/calendar-{user.id}.ics</a><br/>&nbsp;</pre>
             |]
            _ -> [hsx||]

actWarning :: Html
actWarning = case fromFrozenContext :: Maybe SessionData of
    Just (SessionData {actingFor = Just n}) -> [hsx|
      <p><strong class="text-danger">Warning:</strong> You are acting for {userName n}</p>
     |]
    _ -> [hsx||]


signUpRow ::
  Maybe (Registration, PlayDate, Bool) ->
  [(PlayDate, RegOpenness, [RegWithKey])] -> Html
signUpRow signed_up_for upcoming_dates
    | Nothing <- fromFrozenContext @(Maybe SessionData)
    = [hsx| |]
    | otherwise
    = [hsx|
      <div class="row">
       <div class="col-lg-4">
        <div class="card mb-4 box-shadow md-4">
         <div class="card-header">
          <h2>Sign-up</h2>
         </div>
         <div class="card-body">
          {actWarning}
          {signUpForm signed_up_for upcoming_dates}
         </div>
        </div>
       </div>
      </div>
    |]

signUpForm ::
    Maybe (Registration, PlayDate, Bool) ->
    [(PlayDate, RegOpenness, [RegWithKey])] -> Html

signUpForm Nothing upcoming_dates = [hsx|
   {forEach upcoming_dates signUpButton}
  |]
  where
  signUpButton :: (PlayDate, RegOpenness, [RegWithKey]) -> Html
  signUpButton (pd, Open, regs) = [hsx|
   <form method="POST" class="form-group mb-2" action={RegisterAction "main"}>
   <div class="input-group">
      <input type="hidden" name="date" value={inputValue (pd_date pd)}/>
      <button class={cls}>{pd |> pd_date |> renderDate}</button>
   </div>
   </form>
   |]
     where
        cls | isWaitlist (length regs + 1) = "form-control btn-warning" :: Text
            | otherwise                    = "form-control btn-primary"

  signUpButton (pd, Closed, _regs) = [hsx|
   <div class="form-group mb-2">
   <div class="input-group">
     <button class="form-control" disabled="True" title="Registration closed.">{pd |> pd_date |> renderDate}</button>
   </div>
   </div>
   |]

  signUpButton (pd, NotYet _, _regs) = [hsx|
   <div class="form-group mb-2">
   <div class="input-group">
     <button class="form-control" disabled="True" title="Registration not yet open">{pd |> pd_date |> renderDate}</button>
   </div>
   </div>
   |]

signUpForm (Just (reg, pd, can_unregister)) _ = [hsx|
   <p>You are registered for {get #date reg|>renderDate}.</p>
   <p>You can sign up again on {pd_reg_block_over pd |> renderDate}.</p>
   {delete}
   |]
  where
    delete | can_unregister = [hsx|
        <a href={DeleteRegistrationAction "main" (get #id reg)}
           class="js-delete form-control btn btn-warning border"
           data-confirm="Do you want to unregister? Don't forget to tell the WhatsApp group!">Unregister</a>
      |]
           | otherwise = [hsx| |]

renderUpcomingDate :: (PlayDate, RegOpenness, [RegWithKey]) -> Html
renderUpcomingDate (pd, open, regs) = [hsx|
   <div class="col-lg-4">
   <div class="card mb-4 box-shadow md-4">
   <div class="card-header">
   <h2 class="my-0">{pd |> pd_date |> renderDate}</h2>
   </div>
   <div class="card-body">
   <p class="card-text">
   {renderRegDate pd open}
   </p>
   </div>

   <div class="card-body">
     {lines}
   </div>
   </div>
   </div>
|] where
    lines = forEach (zip [1..] (map R regs |> fillUp playSlots E)) renderEntry

renderRegDate pd (NotYet date) = [hsx|
   Registration starts {pd |> pd_reg_opens |> timeAgo}<br/>
|]
renderRegDate pd Open = [hsx|
   Registration started
|]
renderRegDate pd Closed = [hsx|
   Registration closed
|]

renderDate :: UTCTime -> Html
renderDate t = [hsx|
  {t |> utctDay |> dayOfWeek}, {t |> time} {" "::String}
  <small class="text-muted">
  {t |> IHP.View.TimeAgo.date}
  </small>
|]
-- TODO: Not timezone safe  (dateTime uses browser timezone, utctDay UTC)

renderEntry (n, R r) = renderReg n r
renderEntry (n, E) = renderEmpty n

renderPosition :: Int -> Html
renderPosition n | isWaitlist n = [hsx|
     <div class="input-group-prepend">
      <span class="input-group-text" title="on waitlist">?</span>
     </div>
  |]
           | otherwise = [hsx|
     <div class="input-group-prepend">
      <span class="input-group-text bg-success">{n}</span>
     </div>
  |]


renderReg :: Int -> RegWithKey -> Html
renderReg n (reg, has_key) = [hsx|
   <div class="form-group mb-2">
   <div class="input-group">
     {renderPosition n}
     <span class={"input-group-text form-control " <> (if isWaitlist n then "" else "bg-white" :: String)}>
     {regName reg}
     <!-- <span class="small"> {get #createdAt reg |> timeAgo}</span> -->
     </span>
     {nose}
     {key}
   </div>
   </div>
|]
  where
    nose | regIsTrial reg = icon "👃" "Schnupperer"
         | otherwise = mempty

    key | has_key = icon "🔑" "This player brings a key"
        | otherwise = mempty

    icon :: Text -> Text -> Html
    icon symbol title = [hsx|
         <div class="input-group-append">
            <span class="input-group-text form-control bg-white" title={title} style="cursor:default">{symbol}</span>
         </div>
        |]


renderEmpty :: Int -> Html
renderEmpty n = [hsx|
   <div class="form-group mb-2">
   <div class="input-group">
     {renderPosition n}
     <span class={"input-group-text form-control " <> (if isWaitlist n then "" else "bg-white" :: String)}>
     </span>
   </div>
   </div>
|]

fillUp n x xs = xs ++ replicate (n - length xs) x
