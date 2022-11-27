module Web.View.Registrations.Trials where
import Web.View.Prelude
import IHP.View.TimeAgo
import Data.Time.Clock.POSIX


data TrialView = TrialView
  { upcoming_dates :: [(PlayDate, Bool, [Reg])]
  }

data Entry = R (Reg) | E

instance View TrialView where
    html TrialView { .. } = [hsx|
      {signUpRow upcoming_dates}
      <div class="row">
       {forEach upcoming_dates renderUpcomingDate}
      </div>
    |]

signUpRow :: [(PlayDate, Bool, [Reg])] -> Html
signUpRow upcoming_dates
    | Nothing <- fromFrozenContext @(Maybe SessionData)
    = [hsx| |]
    | otherwise
    = [hsx|
      <div class="row">
       <div class="col-lg-4">
        <div class="card mb-4 box-shadow md-4">
         <div class="card-header">
          <h2>Sign-up for trials</h2>
         </div>
         <div class="card-body">
          {signUpForm upcoming_dates}
         </div>
        </div>
       </div>
      </div>
    |]

signUpForm ::
    [(PlayDate, Bool, [Reg])] -> Html

signUpForm upcoming_dates = [hsx|
   <form method="POST" class="mb-2" action={RegisterAction}>
     <div class="form-group">
     <div class="input-group">
        <input class="form-control" type="text" name="trialname" placeholder="Full name" value=""/>
     </div>
     </div>
     {forEach upcoming_dates signUpButton}
   </form>
  |]
  where
  signUpButton :: (PlayDate, Bool, [Reg]) -> Html
  signUpButton (pd, True, regs) = [hsx|
   <div class="form-group">
   <div class="input-group">
      <button type="submit" class={cls} name="date" value={inputValue (pd_date pd)}>{pd |> pd_date |> renderDate}</button>
   </div>
   </div>
   |]
     where
        cls | isWaitlist (length regs + 1) = "form-control btn-warning" :: Text
            | otherwise                    = "form-control btn-primary"

  signUpButton (pd, False, _regs) = [hsx|
   <div class="form-group">
   <div class="input-group">
     <button class="form-control" disabled="True" title="Registration closed.">{pd |> pd_date |> renderDate}</button>
   </div>
   </div>
   |]


renderUpcomingDate :: (PlayDate, Bool, [Reg]) -> Html
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
    lines = forEach (zip [1..] (map R regs |> fillUp playSlots E)) (renderEntry open)

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


renderReg :: Bool -> Int -> Reg -> Html
renderReg open n reg = [hsx|
   <div class="form-group mb-2">
   <div class="input-group">
     {renderPosition n}
     <span class={"input-group-text form-control " <> (if isWaitlist n then "" else "bg-white" :: String)}>
     {regName reg}
     </span>
     {nose}
   </div>
   </div>
|]
  where
    nose | regIsTrial reg = [hsx|
         <div class="input-group-append">
            <a href={DeleteRegistrationAction (get #id reg)} class="js-delete btn btn-secondary bg-white" data-confirm={"Do you want to unregister " <> get #playerName reg <> "?"}>➖</a>
         </div>
      |]
         | otherwise = [hsx||]


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
