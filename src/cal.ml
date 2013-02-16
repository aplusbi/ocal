let calendar_list auth =
  let header = [auth] in
  let url = "https://www.googleapis.com/calendar/v3/users/me/calendarList" in
  Http.get ~header:header url

let event_list auth cal_id =
  let header = [auth] in
  let params = [("timeMin", "2013-02-10T00:00:00-04:00");
                ("timeMax", "2013-03-28T00:00:00-04:00");
                ("maxResults", "2")]
  in
  let url = "https://www.googleapis.com/calendar/v3/calendars/" ^ cal_id ^ "/events" in
  Http.get ~header:header ~params:params url


