let rec f = fun (x: int) ->
    if x <= 0 then -x
    else f (-x)
in
f 42
