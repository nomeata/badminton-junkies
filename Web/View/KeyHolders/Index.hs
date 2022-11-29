module Web.View.KeyHolders.Index where
import Web.View.Prelude
import IHP.View.TimeAgo

data IndexView = IndexView
    { users :: [User]
    , entries :: [Keyholder]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|

    <div class="h-100" id="sessions-new">
        <div class="d-flex align-items-center">
            <div class="w-100">
                <div style="max-width: 400px" class="mx-auto mb-5">
   <div class="card mb-4 box-shadow md-4">
   <div class="card-header">
   <h2 class="my-0">Who has keys</h2>
   </div>

   <div class="card-body">
    {forEach entries (renderEntry users)}
   </div>
   </div>

   </div>
   </div>
   </div>
   </div>
|]

renderEntry :: [User] -> Keyholder -> Html
renderEntry users k = [hsx|
  <form class="form-group" method="POST" action={ChangeKeyHolder}>
   <div class="input-group">
     <div class="input-group-prepend">
      <span class="input-group-text bg-success">{k |> get #keyNumber}</span>
     </div>
     <input name="keyNumber" value={k |> get #keyNumber |> show} type="hidden" />
     <select name="newHolder" required="required" class="form-control form-select" >
          {forEach users userOption}
     </select>
     <div class="input-group-append">
      <button type="submit" class="btn btn-primary">Change</button>
     </div>
  </div>
  </form>
|]
   where
   userOption user = [hsx|
     <option value={user.id |> tshow} selected={user.id  == k.userId}>{userName user}</option>
   |]
