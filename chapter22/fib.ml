let rec fib n =
  if n < 2 then n
  else fib (n - 1) + fib (n - 2)

let fib_array arry =
  let rec fib_rec n =
    (
      Array.set arry n (fib n);
      if n > 0 then fib_rec (n - 1) else arry
    ) in (
      fib_rec (Array.length arry - 1)
    )

let test = fib_array [|0; 0; 0; 0; 0; 0;|]