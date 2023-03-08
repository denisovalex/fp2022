Function
  $ ./demoInterpret.exe << EOF
  > fn x => x
  <fun>

Arithmetic
  $ ./demoInterpret.exe << EOF
  > ((1 + 2) / 3 - 4) * ~5
  15

Tuple
  $ ./demoInterpret.exe << EOF
  > let val f = (fn x => not x) val y = ('a', "abc") in (f false, y) end
  (true, ('a', "abc"))

Array
  $ ./demoInterpret.exe << EOF
  > let val f = fn x => fn arr => x::arr in f 1 [2, 3] end
  [1, 2, 3]

Equality types
  $ ./demoInterpret.exe << EOF
  > let val r = fn y1 => fn y2 => fn x1 => fn x2 => y1 = y2 orelse not (x1 = x2) in r 1 2 3 4 end
  true

Case of
  $ ./demoInterpret.exe << EOF
  > let val f = fn x => fn y => let val id = (fn x => x) val idid = (fn x => id id x) in
  > (case idid x of true => 1 | _ => 0) + (case idid y of 1 => 1 | _ => 0) end
  > in f true 1 end
  2

Array sum
  $ ./demoInterpret.exe << EOF
  > let val rec sum = (fn arr => case arr of [] => 0 | h::t => h + sum t) 
  > in sum [1, 2, 3, 4, 5] end
  15

Factorial
  $ ./demoInterpret.exe << EOF
  > let val rec fix = (fn f => fn x => f (fix f) x) 
  > val fact = (fn self => fn n => if n = 0 then 1 else n * self (n - 1)) 
  > val f = (fn n => fix fact n) 
  > in f 10 end
  3628800

Skip arg
  $ ./demoInterpret.exe << EOF
  > let val f = fn _ => 1 in f true end 
  1

Errors
  $ ./demoInterpret.exe << EOF
  > let val f = fn x => case x of 1 => true in f 2 end
  Runtime error: this pattern-matching is not exhaustive.
  $ ./demoInterpret.exe << EOF
  > 1 / 0
  Runtime error: division by zero.
  $ ./demoInterpret.exe << EOF
  > let val factorial = fn n => if n <= 1 then 1 else n * factorial (n - 1) in factorial 10 end
  Runtime error: unbound value factorial.
  $ ./demoInterpret.exe << EOF
  > let val f = fn x => _ in f true end
  Runtime error: wildcard must not appear on the right-hand side of an expression.
