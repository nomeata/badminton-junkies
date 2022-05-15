module Web.View.Logs.Index where
import Web.View.Prelude
import IHP.View.TimeAgo

data IndexView = IndexView {
    entries :: [Log]
    }

instance View IndexView where
    html IndexView { .. } = [hsx|
    <table class="table">
    <tbody>
    {forEach entries renderEntry}
    </tbody>
    </table>

|]

renderEntry :: Log -> Html
renderEntry l = [hsx|
  <tr>
    <td>{l |> get #createdAt |> timeAgo}</td>
    <td>{l |> get #text}</td>
  </tr>
|]
