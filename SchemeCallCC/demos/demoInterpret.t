Different things
  $ ./demoInterpret.exe <<-EOF
  > (display (+ 1 1 1 1 1 1))
  > EOF
  6 

  $ ./demoInterpret.exe <<-EOF
  > (display (>= 10 (+ 4 2)))
  > EOF
  true 

  $ ./demoInterpret.exe <<-EOF
  > (display (car (list 1 2 3)))
  > (display (cdr (list 1 2 3)))
  > EOF
  1 (2 3) 

  $ ./demoInterpret.exe <<-EOF
  > (display (if (> 5 6) (- 5 6) 'false))
  > EOF
  false 

Quotes
  $ ./demoInterpret.exe <<-EOF
  > (display '(+ 42 (+ 10 20 (* 5 15))))
  > EOF
  (+ 42 (+ 10 20 (* 5 15))) 

  $ ./demoInterpret.exe <<-EOF
  > (display \`(+ 1 ,(+ 3 4)) )
  > EOF
  (+ 1 7) 

Factorial 
  $ ./demoInterpret.exe <<-EOF
  > (define factorial
  >   (lambda (x)
  >     (if (< x 1)
  >       1
  >       (* x (factorial (- x 1))))))
  > (display (factorial 1))
  > (display (factorial 2))
  > (display (factorial 3))
  > (display (factorial 4))
  > EOF
  1 2 6 24 
