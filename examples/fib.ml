let rec fib = fun (n : int) (m : int) (c : int) ->
  if c = 0 then n + m
  else fib (n + m) n (c - 1) in
fib 0 1 10
