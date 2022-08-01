module Web.View.KeyHolders.Index where
import Web.View.Prelude
import IHP.View.TimeAgo

data IndexView = IndexView
    { entries :: [Keyholder]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
    <table class="table">
    <thead>
    <th>№</th>
    <th>Holder</th>
    </thead>
    <tbody>
    {forEach entries renderEntry}
    </tbody>
    </table>

|]

renderEntry :: Keyholder -> Html
renderEntry k = [hsx|
  <tr>
    <td>{k |> get #keyNumber}</td>
    <td>
     <form class="form-group" method="POST" action={ChangeKeyHolder}>
      <div class="input-group">
        <input name="keyNumber" value={k |> get #keyNumber |> show} type="hidden" />
        <input name="newHolder" value={k |> get #holder} required="required" autofocus="autofocus" class="form-control" />
        <div class="input-group-append">
         <button type="submit" class="btn btn-primary">Change</button>
        </div>
     </div>
     </form>
    </td>
  </tr>
|]
