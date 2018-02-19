
let format (seconds : float) : string =
  let minutes = int_of_float (floor (seconds /. 60.)) in
  let seconds' = mod_float seconds 60. in
  let seconds_text = if seconds > 0. then Printf.sprintf "%.2fs" seconds' else "" in
  let minutes_text = if minutes > 0 then Printf.sprintf "%dm " minutes else "" in
  minutes_text ^ seconds_text
