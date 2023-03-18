(** Copyright 2022-2023, Denisov Alexey *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Angstrom
open Ast

let ( >> ) x c = x >>| fun _ -> c

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false
;;

let is_char = function
  | 'a' .. 'z' | 'A' .. 'Z' -> true
  | '+' | '-' | '/' | '*' | '>' | '<' | '=' -> true
  | _ -> false
;;

let spaces = take_while (fun c -> c = ' ' || c = '\n')
let parens p = spaces *> char '(' *> spaces *> p <* spaces <* char ')' <* spaces
let token t = spaces *> string t
let comment = option "" (spaces *> token ";" *> take_till (fun c -> c = '\n') <* spaces)

let number =
  choice [ token "+"; token "-"; token "" ]
  >>= fun sign -> take_while1 is_digit >>| fun num -> int_of_string (sign ^ num)
;;

let is_initial = function
  | '!' | '$' | '%' | '&' | '*' | '/' | ':' | '<' | '=' | '>' | '?' | '~' | '_' | '^' ->
    true
  | c -> is_char c
;;

let is_subsequent = function
  | '.' | '+' | '-' -> true
  | c -> is_char c || is_digit c || is_initial c
;;

let id =
  choice [ token "+"; token "-"; token "..." ]
  <|> (spaces *> satisfy is_initial
      >>= fun i ->
      many (satisfy is_subsequent) >>| fun s -> String.of_seq @@ List.to_seq @@ (i :: s))
;;

type form =
  { definition : form -> definition t
  ; expression : form -> expression t
  }

let f =
  let definition f =
    fix
    @@ fun _ ->
    parens
      (both (token "define" *> id) (spaces *> f.expression f)
      >>| fun (id, expr) -> id, expr)
  in
  let expression _ =
    fix
    @@ fun expr ->
    let bool_ = token "#t" *> return true <|> token "#f" *> return false in
    let const =
      spaces
      *> choice
           [ (number >>| fun n -> Int n)
           ; (bool_ >>| fun b -> Bool b)
           ; (id >>| fun s -> String s)
           ]
    in
    let datum =
      fix (fun datum ->
        let dlist = parens (sep_by spaces datum) >>| fun x -> DList x in
        let dconst = const >>| fun c -> DConst c in
        let dabbr =
          choice [ token "'" >> PQuote; token "`" >> PBackquote; token "," >> PComma ]
          >>= fun p -> datum >>| fun d -> DAbbr (p, d)
        in
        spaces *> (dconst <|> dlist <|> dabbr))
    in
    let quote = token "'" *> (parens datum <|> datum) >>| fun d -> Quote d in
    let unquote =
      spaces *> token "," *> choice [ (id >>| fun id -> Var id); expr ]
      >>| fun e -> QUnquote e
    in
    let _quasiquote =
      fix (fun _quasiquote ->
        spaces
        *> choice
             [ unquote
             ; (const >>| fun c -> QConst c)
             ; (parens (many _quasiquote) >>| fun q -> QList q)
             ; (datum >>| fun d -> QDatum d)
             ]
        >>| fun q -> q)
    in
    let quasiquote = spaces *> token "`" *> (_quasiquote >>| fun q -> Quasiquote q) in
    let if_ =
      parens
        (both (token "if" *> expr) expr
        >>= fun (condition, then_) ->
        option
          (If (condition, then_, None))
          (expr >>| fun else_ -> If (condition, then_, Some else_)))
    in
    (*let formals =
      parens (many id) >>| (fun x -> FormalList x) <|> (id >>| fun x -> Formal x)
    in
    let lambda =
      parens
        (token "lambda" *> formals
        >>= fun formals ->
        many (f.definition f)
        >>= fun defs -> many1 expr >>| fun exprs -> Lambda (formals, defs, exprs))
    in*)
    let func_call =
      parens (both expr (sep_by spaces expr) >>| fun (func, args) -> FuncCall (func, args))
    in
    choice
      [ (*lambda
      ;*) if_
      ; func_call
      ; quote
      ; quasiquote
      ; (number >>| fun n -> Const (Int n))
      ; (bool_ >>| fun b -> Const (Bool b))
      ; (id >>| fun var -> Var var)
      ]
  in
  { definition; expression }
;;

let scheme_parser =
  spaces
  *> many
       (f.definition f
       >>| (fun def -> Def def)
       <|> (f.expression f >>| fun exp -> Expr exp)
       <* comment)
;;

let parse str = parse_string ~consume:All scheme_parser str;;