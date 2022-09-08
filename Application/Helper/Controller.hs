module Application.Helper.Controller
  ( module Application.Helper.Controller
  , module Application.Helper.Both )
  where

import IHP.ControllerPrelude
import Application.Helper.Both

import Web.Types
import Generated.Types

-- Here you can add functions which are available in all your controllers

logMessage :: (?modelContext::ModelContext, ?context::ControllerContext) => Text -> IO Log
logMessage txt = do
    sd <- fromMaybe (error "This should not happen") <$> fromContext
    newRecord @Log
        |> set #text txt
        |> set #userId (user sd |> get #id)
        |> createRecord
