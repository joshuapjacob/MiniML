let rec f = fun (x:int) ->
  let rec g = fun (y:int) ->
    if y <= x then
      g (f y)
    else
      f (-y)
  in
  if x <= 0 then
    42
  else
    g (f (x-1))
in
f 2
