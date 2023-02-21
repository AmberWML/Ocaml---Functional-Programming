type color = Red | Green | Blue

let my_fav = Blue

type weekday = Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             | Sunday

let isWorkDay (d:weekday) : bool = match d with
    | Monday | Tuesday | Wednesday
      | Thursday | Friday -> true
    | _ -> false