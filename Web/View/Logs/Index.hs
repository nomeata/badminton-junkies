module Web.View.Logs.Index where
import Web.View.Prelude
import IHP.View.TimeAgo

data IndexView = IndexView
    { entries :: [Include "userId" Log]
    , pagination :: Pagination
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
    <div>
    {renderPagination pagination}
    </div>
    <table class="table">
    <tbody>
    {forEach entries renderEntry}
    </tbody>
    </table>

|]

renderEntry :: Include "userId" Log -> Html
renderEntry l = [hsx|
  <tr>
    <td>{l |> get #createdAt |> timeAgo}</td>
    <td title={l |> get #userId |> get #fullname}>{l |> get #userId |> userName }</td>
    <td>{l |> get #text}</td>
  </tr>
|]
