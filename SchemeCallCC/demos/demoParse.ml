open My_scheme_call_lib.Parser

let test_program = 
  {|
  
  (define a 10)
  (define b 25)

  (define c (if (> a b) (- a b) (+ a b)))
  
  (display c)
  
  |}

let () =
  match (parse test_program) with
  | Ok _ -> ()
  | Error e -> Printf.printf "%s" e
;;