module Web.View.Registrations.Calendar where

import Web.View.Prelude
import Data.Time.Clock.POSIX
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text.Lazy as T

import Text.ICalendar

data CalendarView = CalendarView
  { now :: UTCTime
  , upcoming_dates :: [(PlayDate, [Reg])]
  , mbUserId :: Maybe (Id User)
  }


renderCalendar :: CalendarView -> LByteString
renderCalendar (CalendarView {..}) = printICalendar def cal
  where
    cal = def
      { vcProdId = ProdId "badjunk" def
      , vcEvents = M.fromList [ ((uidValue (veUID e), def), e) | e <- events]
      }
    events = [ renderEvent mbUserId now pd regs | (pd, regs) <- upcoming_dates ]

renderEvent :: Maybe (Id User) -> UTCTime -> PlayDate -> [Reg] -> VEvent
renderEvent mbUserId now pd regs = VEvent
      { veDTStamp = DTStamp now def
      , veUID = UID (T.fromStrict $ show (pd_date pd) <> "@badjunk.nomeata.de") def
      , veClass = def
      , veDTStart = Just $ DTStartDateTime (UTCDateTime (pd_date pd)) def
      , veCreated = Nothing
      , veDescription = Just $ Description desc Nothing Nothing def
      , veGeo = Just $ Geo 47.98681 7.86847 def
      , veLastMod  = Nothing
      , veLocation  = Just $ Location "Emil Thoma-Halle" def def def
      , veOrganizer  = Nothing
      , vePriority = def
      , veSeq = def
      , veStatus  = Nothing
      , veSummary = Just $ Summary summary def def def
      , veTransp = def
      , veUrl  = Nothing
      , veRecurId  = Nothing
      , veRRule  = S.empty
      , veDTEndDuration  = Just (Left (DTEndDateTime (UTCDateTime (pd_until pd)) def))
      , veAttach  = S.empty
      , veAttendee  = S.empty
      , veCategories  = S.empty
      , veComment  = S.empty
      , veContact  = S.empty
      , veExDate  = S.empty
      , veRStatus  = S.empty
      , veRelated  = S.empty
      , veResources  = S.empty
      , veRDate  = S.empty
      , veAlarms  = S.empty
      , veOther  = S.empty
      }
  where
    pos | Just uid <- mbUserId
        = listToMaybe [ n | (n, reg) <- zip [1..playSlots+1] regs, Just u <- pure reg.playerUser, u.id == uid ]
        | otherwise
        = Nothing
    summary = T.fromStrict $ case pos of
      Nothing ->            "🏸 Badminton! (" <> show (length regs) <> " 🙋)"
      Just n | n <= 9 ->    "🏸 Badminton! (" <> show n <> "/" <> show (length regs) <> " 🙋)"
             | otherwise -> "🏸 Badminton! (" <> show n <> "/" <> show (length regs) <> " ⏳)"
    desc = "Registered are:\n" <> T.concat [
        T.fromStrict $ show n <> ". " <> regName reg <> "\n"
     | (n, reg) <- zip [1..playSlots+1] regs ]
