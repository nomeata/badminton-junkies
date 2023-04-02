module Web.View.Registrations.Stats where
import Web.View.Prelude

import qualified Data.Text as T

data StatsView = StatsView
    { regCount :: [(Either User Text, Int)]
    }

instance View StatsView where
    html StatsView { .. } = [hsx|
        <h1>Registrations <small>(last three monhts)</small></h1>
        <table class="table">
        <tbody>
        {forEach regCount row}
        </tbody>
        </table>
      |]
      -- This is ugly...
      where
        max = maximum (map snd regCount)
        rel c = c * 100 `div` max
        name = either userName ("👃 "  <>)
        row (mbn, c) = [hsx|
          <tr>
          <td style="white-space: nowrap">{name mbn}</td>
          <td style="text-align: right">{c}</td>
          <td style="width:100%"><div style={s}>&nbsp;</div></td>
          </tr>
        |]
          where s = "background-color:blue; border-radius: 5px; \
                    \width:" <> tshow (rel c) <> "%; height:100%"
