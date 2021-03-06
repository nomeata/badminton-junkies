{-# LANGUAGE ViewPatterns #-}
module Application.BuhlAuth where

import IHP.ViewPrelude

import Network.Wreq
import Control.Lens
import Data.Aeson.Lens
import Control.Monad.Trans.Except
import Text.HTML.TagSoup
import Text.URI
import Data.ByteString.UTF8 as BSU
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

badmintonJunkiesId :: Integer
badmintonJunkiesId = 42249

buhlLogin :: Text -> Text -> IO (Either Text Text)
buhlLogin username password = runExceptT $ do
    r <- liftIO $ post "https://www.buhl.de/mein-buhlkonto/wp-admin/admin-ajax.php"
        [ "action" `partText` "LoginUser"
        , "post_type" `partText` "POST"
        , "param[0][name]" `partText` "eml-user"
        , "param[0][value]" `partText` username
        , "param[1][name]" `partText` "psw-user"
        , "param[1][value]" `partText` password
        , "param[2][name]" `partText` "passed-get-param"
        , "param[2][value]" `partText` ""
        , "param[3][name]" `partText` "passed-ls-param"
        , "param[3][value]" `partText` ""
        , "param[4][name]" `partText` "rdr-buhlparam"
        , "param[4][value]" `partText` "1050105"
        ]
    success <- orErr "admin-ajax.php did not return a success field" $
        r ^? responseBody . key "success" . _Bool
    unless success $ do
        msg <- orErr "admin-ajax.php signalled failure, but without a message field" $
            r ^? responseBody . key "message" . _String
        throwE msg
    redirHTML <- orErr "admin-ajax.php did not return a redirection" $
        r ^? responseBody . key "redirection" . _String
    (actionUrl, ticket) <- case parseTags (redirHTML :: Text) of
      [ TagOpen "form" [("action", actionUrl), ("method", "post"), ("id", _)]
        , TagOpen "input" [("type", "hidden"), ("value", ticket), ("name", "BuhlTicket")]
        , TagClose "input"
        , TagClose "form"] -> pure (actionUrl, ticket)
      _ -> throwE "admin-adjax.php changes the redirection field, please let Joachim know"

    -- Annoyingly wreq insists on following redirects,
    -- even though we only care about the first request
    r <- liftIO $ customHistoriedPayloadMethod "POST"
      (T.unpack actionUrl) ["BuhlTicket" := ticket]

    (_, r) <- orErr "No requests returned?" $ listToMaybe $ r ^. hrRedirects
    url <- orErr "token did not return a Location header" $ r ^? responseHeader "Location"
    uri <- mkURI (T.decodeUtf8 url)
    token <- case uriQuery uri of
      QueryParam (unRText -> "token") (unRText -> token) :_ -> pure token
      _ -> throwE "Unexpected URL in response to token request"

    r <- liftIO $ getWith (defaults & auth ?~ oauth2Bearer (T.encodeUtf8 token))
        "https://api.meinverein.de/protected/accounts?isAdminToolSession=false"
    first_name <- orErr "accounts endpoint did not reveal first name" $
        r ^? responseBody . key "user" . key "firstName" . _String
    last_name <- orErr "accounts endpoint did not reveal last name" $
        r ^? responseBody . key "user" . key "lastName" . _String

    clubs <- orErr "accounts endpoint did not lists clubs" $
        r ^? responseBody . key "data" . key "clubs" . _Array

    unless (Just badmintonJunkiesId `elem` fmap (\c -> c ^? key "id" . _Integer) clubs) $
        throwE "It looks like you are not a member yet."

    return $ first_name <> " " <> last_name

orErr msg Nothing = throwE msg
orErr msg (Just x) = pure x
