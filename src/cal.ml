open Batteries
open BatPervasives
open Yojson.Basic
open Utils

type event = {
  summary: string;
  start : Unix.tm;
  reminders : int list;
}


let calendar_list auth =
  let header = [auth] in
  let url = "https://www.googleapis.com/calendar/v3/users/me/calendarList" in
  Http.get ~header:header url

let parse_date json =
  let o = get_obj json in
  BatList.assoc "dateTime" o
  |> get_string
  |> Datetime.parse

let parse_reminder json =
  let o = get_obj json in
  BatList.assoc "minutes" o |> get_int

let parse_reminders json =
  let o = get_obj json in
  BatList.assoc "overrides" o
  |> get_list
  |> BatList.map parse_reminder

let parse_event json =
  let o = get_obj json in
  let summary = BatList.assoc "summary" o
                |> get_string in
  let start = BatList.assoc "start" o
              |> parse_date in
  let reminders = BatList.assoc "reminders" o
                  |> parse_reminders in
  {
    summary = summary;
    start = start;
    reminders = reminders;
  }

let parse_event_list json =
  let o = get_obj json in
  let i = BatList.assoc "items" o
          |> get_list in
  BatList.map parse_event i

let event_list auth cal_id =
  let header = [auth] in
  let params = [("timeMin", "2013-02-10T00:00:00-04:00");
                ("timeMax", "2013-03-28T00:00:00-04:00");
                ("maxResults", "2")]
  in
  let url = "https://www.googleapis.com/calendar/v3/calendars/" ^ cal_id ^ "/events" in
  match Http.get ~header:header ~params:params url with
  | (200, _, body) ->
    let events = from_string body
                 |> parse_event_list in
    (200, events)
  | (status, _, _) -> (status, [])

