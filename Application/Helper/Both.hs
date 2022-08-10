module Application.Helper.Both where

import IHP.ControllerPrelude

playSlots :: Int
playSlots = 9

isWaitlist :: Int -> Bool
isWaitlist n = n > playSlots

