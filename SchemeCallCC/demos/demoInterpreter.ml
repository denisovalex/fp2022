open My_scheme_call_lib.Interpreter

let test_program = 
  {|
  
  (define a 10)
  (define b 25)

  (define c (if (> a b) (- a b) (+ a b)))
  
  (display c)
  
  |}

let () = 
  match (parse_and_interpr_prog test_program) with
  | Ok (_, return_value) -> Printf.printf "%s" (Interpret.show_value return_value)
  | Error e -> Printf.printf "%s" e
;;