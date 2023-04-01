module Web.View.Layout (defaultLayout, Html) where

import IHP.ViewPrelude
import IHP.Environment
import qualified Text.Blaze.Html5            as H
import qualified Text.Blaze.Html5.Attributes as A
import Generated.Types
import IHP.Controller.RequestContext
import Web.Types
import Web.Routes
import Application.Helper.View

defaultLayout :: Html -> Html
defaultLayout inner = H.docTypeHtml ! A.lang "en" $ [hsx|
<head>
    {metaTags}

    {stylesheets}
    {scripts}

    <title>{pageTitleOrDefault "Badminton Junkies"}</title>
</head>
<body>

    <div class="d-flex flex-column flex-md-row align-items-center p-3 px-md-4 mb-3 bg-white border-bottom box-shadow">
      <h5 class="my-0 mr-md-auto font-weight-normal">Badminton Junkies</h5>
      <div class="my-2 my-md-0 mr-md-3">
        {greeting}
      </div>
      <nav class="my-2 my-md-0 mr-md-3">
        <a href={RegistrationsAction} title="Register"       class={classes ["p-2 mx-1 btn text-dark border ", ("border-primary", isActivePath RegistrationsAction)]}>🏸</a>
        <a href={TrialsAction}        title="Trial signup"   class={classes ["p-2 mx-1 btn text-dark border ", ("border-primary", isActivePath TrialsAction)]}>👃</a>
        <a href={KeyHoldersAction}    title="Keys"           class={classes ["p-2 mx-1 btn text-dark border ", ("border-primary", isActivePath KeyHoldersAction)]}>🔑</a>
        <a href={PastAction Nothing}  title="Past days"      class={classes ["p-2 mx-1 btn text-dark border ", ("border-primary", isActivePath (PastAction Nothing))]}>📅</a>
        <a href={LogsAction Nothing}  title="Log of actions" class={classes ["p-2 mx-1 btn text-dark border ", ("border-primary", isActivePath (LogsAction Nothing))]}>📓</a>
        <a href={StatsAction}         title="Statistis     " class={classes ["p-2 mx-1 btn text-dark border ", ("border-primary", isActivePath StatsAction)]}>📊</a>
        <a href={EditSessionAction}   title="Settings"       class={classes ["p-2 mx-1 btn text-dark border ", ("border-primary", isActivePath EditSessionAction)]}>🛠</a>
      </nav>
    </div>

    <div class="container">
        {renderFlashMessages}
        {inner}
    </div>
</body>
|]

greeting :: Html
greeting =
    case fromFrozenContext :: Maybe SessionData of
        Nothing ->
            [hsx|
                  <a class="p-2 btn btn-outline-primary" href={EditSessionAction}>Log in</a>
            |]
        Just sd -> case actingFor sd of
            Just u' ->
                [hsx|
                      <span class="p-2">Hello, <s>{userName (user sd)}</s> {userName u'}!</span>
                |]
            Nothing ->
                [hsx|
                      <span class="p-2">Hello, {userName (user sd)}!</span>
                |]

-- The 'assetPath' function used below appends a `?v=SOME_VERSION` to the static assets in production
-- This is useful to avoid users having old CSS and JS files in their browser cache once a new version is deployed
-- See https://ihp.digitallyinduced.com/Guide/assets.html for more details

stylesheets :: Html
stylesheets = [hsx|
        <link rel="stylesheet" href={assetPath "/vendor/bootstrap.min.css"}/>
        <link rel="stylesheet" href={assetPath "/vendor/flatpickr.min.css"}/>
        <link rel="stylesheet" href={assetPath "/app.css"}/>
    |]

scripts :: Html
scripts = [hsx|
        {when isDevelopment devScripts}
        <script src={assetPath "/vendor/jquery-3.6.0.slim.min.js"}></script>
        <script src={assetPath "/vendor/timeago.js"}></script>
        <script src={assetPath "/vendor/popper.min.js"}></script>
        <script src={assetPath "/vendor/bootstrap.min.js"}></script>
        <script src={assetPath "/vendor/flatpickr.js"}></script>
        <script src={assetPath "/vendor/morphdom-umd.min.js"}></script>
        <script src={assetPath "/vendor/turbolinks.js"}></script>
        <script src={assetPath "/vendor/turbolinksInstantClick.js"}></script>
        <script src={assetPath "/vendor/turbolinksMorphdom.js"}></script>
        <script src={assetPath "/helpers.js"}></script>
        <script src={assetPath "/ihp-auto-refresh.js"}></script>
        <script src={assetPath "/app.js"}></script>
    |]

devScripts :: Html
devScripts = [hsx|
        <script id="livereload-script" src={assetPath "/livereload.js"} data-ws={liveReloadWebsocketUrl}></script>
    |]

metaTags :: Html
metaTags = [hsx|
    <meta charset="utf-8"/>
    <meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no"/>
    <meta property="og:title" content="Bad Junk!"/>
    <meta property="og:type" content="website"/>
    <meta property="og:url" content="https://badjunk.nomeata.de"/>
    <meta property="og:description" content="Badminton Junkies registration website"/>
    {autoRefreshMeta}
|]
