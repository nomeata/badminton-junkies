module Web.View.Registrations.Index where
import Web.View.Prelude
import IHP.View.TimeAgo
import Data.Time.Clock.POSIX

data PlayDate = PlayDate
  { pd_date           :: UTCTime
  , pd_reg_opens      :: UTCTime
  , pd_reg_block_over :: UTCTime
  }

data IndexView = IndexView
  { signed_up_for :: Maybe (Registration, PlayDate, Bool)
  , upcoming_dates :: [(PlayDate, Bool, [Entry])]
  }

data Entry = R Registration | E

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
        <p>You can sign up for one play time at a time. If you have just played, wait until 20:30 before signing up for the next play time.</p>
        <p>You can unregister. People on the waitlist will then move up. Please also message the group to tell them that a spot is now open.</p>
        <p>To indicate that you have a key, click on  🏸 to turn it into a 🔑.</p>
        <p>To change your nickname or register other people, click the 🛠 button in the top-right corner.</p>
        <p>The system keeps a log of registrations and removals.</p>
        </div>
        </div>
       </div>

       <div class="col-lg-4">
        <div class="card mb-4 box-shadow md-4">
        <div class="card-header">
        <h2>TODO</h2>
        </div>
        <div class="card-body">
        <p>This website is work-in-progress. The following features are missing:</p>
        <ul>
          <li>Auto-complete player names</li>
          <li>Viewing past line-ups</li>
        </ul>
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
            breadcrumb = renderBreadcrumb
                [ breadcrumbLink "Registrations" RegistrationsAction
                ]

actWarning :: Html
actWarning = case fromFrozenContext :: Maybe SessionData of
    Just (SessionData {actingFor = Just n}) -> [hsx|
      <p><strong class="text-danger">Warning:</strong> You are acting for {n}</p>
     |]
    _ -> [hsx||]


signUpRow :: Maybe (Registration, PlayDate, Bool) -> [(PlayDate, Bool, [Entry])] -> Html
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

signUpForm :: Maybe (Registration, PlayDate, Bool) -> [(PlayDate, Bool, [Entry])] -> Html

signUpForm Nothing upcoming_dates = [hsx|
   {forEach upcoming_dates signUpButton}
  |]
  where
  signUpButton :: (PlayDate, Bool, [Entry]) -> Html
  signUpButton (pd, True, regs) = [hsx|
   <form method="POST" class="form-group mb-2" action={RegisterAction}>
   <div class="input-group">
      <input type="hidden" name="date" value={inputValue (pd_date pd)}/>
      <button class="form-control btn-primary">{pd |> pd_date |> renderDate}</button>
   </div>
   </form>
   |]
  signUpButton (pd, False, regs) = [hsx|
   <div class="form-group mb-2">
   <div class="input-group">
     <button class="form-control" disabled="True" title="Registration closed.">{pd |> pd_date |> renderDate}</button>
   </div>
   </div>
   |]

signUpForm (Just (reg, pd, open)) _ = [hsx|
   <p>You are registered for {get #date reg|>renderDate}.</p>
   <p>You can sign up again at {pd_reg_block_over pd |> renderDate}.</p>
   {delete}
   |]
  where
    delete | open = [hsx|
        <a href={DeleteRegistrationAction (get #id reg)}
           class="js-delete form-control btn btn-warning border"
           data-confirm="Do you want to unregister?">Unregister</a>
      |]
           | otherwise = [hsx| |]

renderUpcomingDate :: (PlayDate, Bool, [Entry]) -> Html
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
   {forEach (zip [1..] regs) (renderEntry open)}
   </div>
   </div>
   </div>
|]

renderRegDate pd True = [hsx|
   Start of registration: {pd |> pd_reg_opens |> timeAgo}<br/>
|]
renderRegDate pd False = [hsx|
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

renderEntry open (n, R r) = renderReg open n r
renderEntry open (n, E) = renderEmpty n

isWaitlist n = n > 9

renderPosition :: Integer -> Html
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


renderReg :: Bool -> Integer -> Registration -> Html
renderReg open n reg = [hsx|
   <div class="form-group mb-2">
   <div class="input-group">
     {renderPosition n}
     <span class={"input-group-text form-control " <> (if isWaitlist n then "" else "bg-white" :: String)}>
     {get #playerName reg}
     <!-- <span class="small"> {get #createdAt reg |> timeAgo}</span> -->
     </span>
     {renderRacket}
   </div>
   </div>
|]
  where
    renderRacket | get #hasKey reg = renderRacketOrKey "🔑" "False" "no key"
                 | otherwise       = renderRacketOrKey "🏸" "True" "a key"

    renderRacketOrKey symbol otherVal confirm = [hsx|
         <div class="input-group-append">
           <form method="POST" action={SetKeyRegistrationAction (get #id reg)}>
            <input type="hidden" name="hasKey" value={otherVal::Text}/>
            <button type="submit" class="btn btn-light border" title={"Does " <> get #playerName reg <> " have " <> confirm <> "?"}>
             {symbol :: Text}
            </button>
           </form>
         </div>
        |]

renderEmpty :: Integer -> Html
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
