module Web.View.Registrations.Past where
import Web.View.Prelude
import IHP.View.TimeAgo
import Data.Time.Clock.POSIX

type RegWithKey = (Reg, Bool)

data PastView = PastView
  { dates :: [(UTCTime, [Reg])]
  , pagination :: Pagination
  }

data Entry = R Reg | E

instance View PastView where
    html PastView { .. } = [hsx|
      <div>
       {renderPagination pagination}
      </div>
      <div class="row">
       {forEach dates renderRegDate}
      </div>
    |]

renderRegDate :: (UTCTime, [Reg]) -> Html
renderRegDate (d, regs) = [hsx|
   <div class="col-lg-4">
   <div class="card mb-4 box-shadow md-4">
   <div class="card-header">
   <h2 class="my-0">{d |> renderDate}</h2>
   </div>

   <div class="card-body">
     {lines}
   </div>
   </div>
   </div>
|] where
    lines = forEach (zip [1..] (map R regs |> fillUp playSlots E)) renderEntry


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


renderReg :: Int -> Reg -> Html
renderReg n reg = [hsx|
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
    nose | regIsTrial reg = icon "👃" "Schnupperer"
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
