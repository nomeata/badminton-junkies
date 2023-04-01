module Web.View.Registrations.Stats where
import Web.View.Prelude

import qualified Data.Text as T

data StatsView = StatsView
    { regCount :: [(Maybe User, Int)]
    }

instance View StatsView where
    html StatsView { .. } = [hsx|
        <canvas id="myChart" height="200%"></canvas>
 	<script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
        {dataDef |> preEscapedToHtml}
        <script>
            const data = {
                    labels: names,
                    datasets: [{
                            label: 'Play times',
                            data: counts,
                            backgroundColor: 'blue',
                    }]
            };

            const config = {
              type: 'bar',
              data: data,
              options: {
                animation: false,
                indexAxis: 'y',
                // Elements options apply to all of the options unless overridden in a dataset
                // In this case, we are setting the border of each horizontal bar to be 2px wide
                elements: {
                  bar: {
                    borderWidth: 2,
                  }
                },
                responsive: true,
                plugins: {
                  legend: {
                    display: false,
                    // position: 'right',
                  },
                  title: {
                    text: 'Registrations last three months'
                  }
                }
              },
            };

            const ctx = document.getElementById('myChart').getContext('2d');
            new Chart(ctx, config);
        </script>
      |]
      -- This is ugly...
      where names = T.intercalate ", " $ map (quote . maybe "👃" userName . fst) regCount
            quote s = "'" <> T.concatMap esc s <> "'"
            esc '\\' = "\\\\"
            esc '\'' = "\\'"
            esc '\"' = "\\\""
            esc '<' = "\\u003c"
            esc c = T.singleton c
            counts = T.intercalate ", " $ map (tshow . snd) regCount
            dataDef = "<script>const names = [" <> names <> "]; counts = [" <> counts <> "]</script>"


renderEntry :: Include "userId" Log -> Html
renderEntry l = [hsx|
  <tr>
    <td>{l |> get #createdAt |> timeAgo}</td>
    <td title={l |> get #userId |> get #fullname}>{l |> get #userId |> userName }</td>
    <td>{l |> get #text}</td>
  </tr>
|]
