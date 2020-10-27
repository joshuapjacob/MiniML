let rec f = fun (x:int) ->
  if x = 0 then 1
  else f (x-1)
in f(1)