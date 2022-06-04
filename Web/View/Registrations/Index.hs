module Web.View.Registrations.Index where
import Web.View.Prelude
import IHP.View.TimeAgo
import Data.Time.Clock.POSIX

data PlayDate = PlayDate
  { pd_date           :: UTCTime
  , pd_reg_opens      :: UTCTime
  , pd_reg_block_over :: UTCTime
  }

data IndexView = IndexView {
    upcoming_dates :: [(PlayDate, Bool, [Entry])]
    }

data Entry = R Registration | F (Maybe Registration) | E

instance View IndexView where
    html IndexView { .. } = [hsx|
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
        <p>Enter your name and press ➕ to add yourself to one of the play times. The first 9 members to register get to play, if you add yourself later you are on the waitlist.</p>
        <p>You can sign up for one play time at a time. If you have just played, wait until 20:30 before signing up for the next play time. Please do not cheat by using different names.</p>
        <p>To unregister yourself, press ➖. People on the waitlist will then move up. Please also message the group to tell them that a spot is now open.</p>
        <p>To indicate that you have a key, click on  🏸 to turn it into a 🔑.</p>
        <p>You <em>can</em> add or remove other people! This is intentional, e.g. to register as a pair, or remove someone else when they asked you to. Please do not abuse this.</p>
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
          <li>Viewing earlier line-ups</li>
          <li>Pagination and pruning of log</li>
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
renderEntry open (n, F r) = newRegForm n r
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
     {renderDelete}
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

    renderDelete | open = [hsx|
         <div class="input-group-append">
           <a href={DeleteRegistrationAction (get #id reg)} class="js-delete btn btn-light border" data-confirm={"Do you want to unregister " <> get #playerName reg <> "?"}>➖</a>
         </div>
        |]
                 | otherwise = [hsx| |]

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

newRegForm :: Integer -> Maybe Registration -> Html
newRegForm n Nothing = [hsx|
   <div class="form-group mb-2">
   <div class="input-group">
     {renderPosition n}
     <span class={"input-group-text form-control " <> (if isWaitlist n then "" else "bg-white" :: String)}>
     <a class="" href={NewSessionAction}>Log in</a> to register
     </span>
   </div>
   </div>
|]

newRegForm n (Just reg) = formForWithOptions reg
  (modify #formClass (<> " form-group mb-2") .
   set #formId ("add-reg-" <> date_id (get #date reg)))
  [hsx|
   {(hiddenField #date) { disableGroup = True }}
   <div class="input-group">
     {renderPosition n}
     {(textField #playerName) { placeholder = "Name of player to add", required = True, disableLabel = True, disableGroup = True }}
     <div class="input-group-append">
      {submitButton { label = "➕" } }
     </div>
   </div>
  |]
  where date_id = tshow @Integer . round . utcTimeToPOSIXSeconds


fillUp n x xs = xs ++ replicate (n - length xs) x
