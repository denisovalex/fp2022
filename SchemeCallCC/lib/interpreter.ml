(** Copyright 2022-2023, Denisov Alexey and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Ast
open Parser

let ( let* ) m f = Result.bind m f
let return = Result.ok

type value =
  | VVoid
  | VString of string
  | VInt of int
  | VBool of bool
  | VVar of id
  | VList of value list
  | VExprList of expression list
  | VLambda of formals * definition list * expression list * context

and context = { vars : var list }

and var =
  { id : id
  ; value : value
  }

module Interpret = struct
  let bin_ops =
    [ "+"
    ; "*"
    ; "-"
    ; "/"
    ; "="
    ; ">"
    ; "<"
    ; ">="
    ; "<="
    ; "cons"
    ; "list"
    ; "append"
    ; "apply"
    ; "newline"
    ]
  ;;

  let un_ops =
    [ "not"
    ; "zero?"
    ; "positive?"
    ; "negative?"
    ; "odd?"
    ; "even?"
    ; "abs"
    ; "boolean?"
    ; "integer?"
    ; "number?"
    ; "procedure?"
    ; "pair?"
    ; "list?"
    ; "null?"
    ; "car"
    ; "cdr"
    ; "length"
    ; "display"
    ]
  ;;

  (*___________________Helpers___________________*)

  let find_var ctx id =
    List.find_map
      (fun v ->
        match v.id with
        | v_id when String.equal v_id id -> Some v
        | _ -> None)
      ctx.vars
  ;;

  let update_ctx ctx var =
    match find_var ctx var.id with
    | Some _ ->
      let vars =
        var
        :: List.find_all
             (fun v ->
               match v.id with
               | v_id when String.equal v_id var.id -> false
               | _ -> true)
             ctx.vars
      in
      return { vars }
    | None ->
      let vars = var :: ctx.vars in
      return { vars }
  ;;

  let batch_eval : 'a. 'a list -> ('a -> (value, 'b) result) -> (value list, 'b) result =
   fun (exprs : 'a list) (f : 'a -> (value, 'b) result) : (value list, 'b) result ->
    let rec helper = function
      | [] -> return []
      | hd :: tl ->
        let* r = f hd in
        let* t = helper tl in
        return (List.cons r t)
    in
    helper exprs
 ;;

  let rec show_value = function
    | VVoid -> "#<void>"
    | VBool b -> Format.sprintf "%b" b
    | VInt d -> Format.sprintf "%d" d
    | VList l ->
      let rec helper = function
        | [ y ] -> show_value y
        | hd :: tl -> Format.sprintf "%s %s" (show_value hd) (helper tl)
        | _ -> ""
      in
      Format.sprintf "(%s)" @@ helper l
    | VString s -> Format.sprintf "%s" s
    | VLambda _ -> "#<lambda>"
    | VExprList _ -> "#<vexprlist>"
    | VVar _ -> "#<procedure>"
  ;;

  (*___________________Do unary operation___________________*)

  let rec do_display arg =
    print_string (Format.sprintf "%s " (show_value arg));
    return VVoid

  and do_un_op ctx op args =
    match args with
    | [ arg ] ->
      let* arg = eval_expr ctx arg in
      (match op, arg with
       | "not", VBool x -> return (VBool (not x))
       | "zero?", VInt x -> return (VBool (x = 0))
       | "positive?", VInt x -> return (VBool (x > 0))
       | "negative?", VInt x -> return (VBool (x < 0))
       | "odd?", VInt x -> return (VBool (x mod 2 = 1))
       | "even?", VInt x -> return (VBool (x mod 2 = 0))
       | "abs", VInt x -> return (VInt (abs x))
       | "boolean?", x ->
         (match x with
          | VBool _ -> return (VBool true)
          | _ -> return (VBool false))
       | "integer?", x ->
         (match x with
          | VInt _ -> return (VBool true)
          | _ -> return (VBool false))
       | "number?", x ->
         (match x with
          | VInt _ -> return (VBool true)
          | _ -> return (VBool false))
       | "procedure?", x ->
         (match x with
          | VVar v when List.mem v (bin_ops @ un_ops) -> return (VBool true)
          | VLambda _ -> return (VBool true)
          | _ -> return (VBool false))
       | "pair?", x ->
         (match x with
          | VList xs when List.length xs != 0 -> return (VBool true)
          | _ -> return (VBool false))
       | "list?", x ->
         (match x with
          | VList _ -> return (VBool true)
          | _ -> return (VBool false))
       | "null?", x ->
         (match x with
          | VList [] -> return (VBool true)
          | _ -> return (VBool false))
       | "car", VList l ->
         (match l with
          | hd :: _ -> return hd
          | [] -> Error (Format.sprintf "Exception in car: () is not a pair\n"))
       | "cdr", VList l ->
         (match l with
          | _ :: tl -> return (VList tl)
          | [] -> Error (Format.sprintf "Exception in cdr: () is not a pair\n"))
       | "length", VList l -> return (VInt (List.length l))
       | "display", v -> do_display v
       | _ -> Error (Format.sprintf "Invalid argument type of <%s> operator\n" op))
    | _ -> Error (Format.sprintf "Incorrect argument count of <%s> operator\n" op)

  (*___________________Do binary operation___________________*)

  and do_arithmetic ctx op args acc =
    match args with
    | [] -> return (VInt acc)
    | hd :: tl ->
      let* arg = eval_expr ctx hd in
      (match arg with
       | VInt n -> do_arithmetic ctx op tl (op acc n)
       | _ -> Error (Format.sprintf "Invalid argument type of arithmetic operator\n"))

  and do_arithmetic2 ctx op args acc =
    match args with
    | [] -> Error (Format.sprintf "Incorrect argument count of arithmetic operator\n")
    | [ x ] -> do_arithmetic ctx op [ Const (Int acc); x ] acc
    | hd :: tl ->
      let* head = eval_expr ctx hd in
      (match head with
       | VInt head -> do_arithmetic ctx op tl head
       | _ -> Error (Format.sprintf "Invalid argument type of arithmetic operator\n"))

  and do_comparison ctx op args =
    let rec helper first = function
      | hd :: tl ->
        let* second = eval_expr ctx hd in
        (match second with
         | VInt second ->
           if op first second then helper second tl else return (VBool false)
         | _ -> Error (Format.sprintf "Invalid argument type of comparasion operator\n"))
      | [] -> return (VBool true)
    in
    match args with
    | hd :: tl ->
      let* second = eval_expr ctx hd in
      (match second with
       | VInt first -> helper first tl
       | _ -> Error (Format.sprintf "Invalid argument type of comparasion operator\n"))
    | [] -> Error (Format.sprintf "Incorrect argument count of comparasion operator\n")

  and do_cons ctx = function
    | [ f; s ] ->
      let* fe = eval_expr ctx f in
      let* se = eval_expr ctx s in
      (match se with
       | VList l -> return (VList (fe :: l))
       | VExprList el ->
         let* r = batch_eval el (eval_expr ctx) in
         return (VList (fe :: r))
       | _ -> Error (Format.sprintf "Invalid argument type of cons operator\n"))
    | _ -> Error (Format.sprintf "Incorrect argument count of cons operator\n")

  and do_list ctx args =
    let* l = batch_eval args (eval_expr ctx) in
    return (VList l)

  and do_bin_op ctx op args =
    match op with
    | "+" -> do_arithmetic ctx ( + ) args 0
    | "-" -> do_arithmetic2 ctx ( - ) args 0
    | "*" -> do_arithmetic ctx ( * ) args 1
    | "/" -> do_arithmetic2 ctx ( / ) args 1
    | "=" -> do_comparison ctx ( = ) args
    | ">" -> do_comparison ctx ( > ) args
    | "<" -> do_comparison ctx ( < ) args
    | ">=" -> do_comparison ctx ( >= ) args
    | "<=" -> do_comparison ctx ( <= ) args
    | "cons" -> do_cons ctx args
    | "list" -> do_list ctx args
    | _ -> Error (Format.sprintf "Wrong operator <%s>\n" op)

  (*___________________Form evaluators___________________*)

  and eval_var ctx id =
    match find_var ctx id with
    | Some var -> return var.value
    | None -> return (VVar id)

  and eval_const = function
    | Int i -> VInt i
    | String s -> VString s
    | Bool b -> VBool b

  and eval_func_call ctx func args =
    let* op = eval_expr ctx func in
    match op with
    | VVar var when List.mem var un_ops -> do_un_op ctx var args
    | VVar var -> do_bin_op ctx var args
    (*| VLambda (formals, defs, exprs, closure) ->
      eval_lambda ctx closure formals defs exprs args *)
    | _ -> Error (Format.sprintf "Wrong operator <%s>\n" (show_value op))

  and eval_if ctx cond then_ else_ =
    let* cond =
      match eval_expr ctx cond with
      | Error e -> Error e
      | Ok (VBool false) -> return false
      | _ -> return true
    in
    if cond
    then eval_expr ctx then_
    else (
      match else_ with
      | None -> return VVoid
      | Some e -> eval_expr ctx e)

  and eval_datum = function
    | DConst c -> eval_const c
    | DList l -> VList (List.map eval_datum l)
    | DAbbr (_, d) -> eval_datum d

  and eval_quasiquote ctx = function
    | QConst c -> return (eval_const c)
    | QDatum d -> return (eval_datum d)
    | QList ql ->
      let* res = batch_eval ql (eval_quasiquote ctx) in
      return (VList res)
    | QUnquote e -> eval_expr ctx e

  and eval_expr ctx = function
    | Var id -> eval_var ctx id
    | Const c -> return (eval_const c)
    | FuncCall (func, args) -> eval_func_call ctx func args
    (*| Lambda (formals, defs, exprs) -> return (VLambda (formals, defs, exprs, ctx))*)
    | If (cond, then_, else_) -> eval_if ctx cond then_ else_
    | Quote q -> return (eval_datum q)
    | Quasiquote q -> eval_quasiquote ctx q

  and eval_def ctx id expr =
    let* value = eval_expr ctx expr in
    let* new_ctx = update_ctx ctx { id; value } in
    return new_ctx

  and eval_form ctx = function
    | Def (id, expr) ->
      let* new_ctx = eval_def ctx id expr in
      return (new_ctx, VVoid)
    | Expr expr ->
      let* return_value = eval_expr ctx expr in
      return (ctx, return_value)
  ;;

  (*___________________Program interpretator___________________*)

  let interpr_prog ast =
    let ctx = { vars = [] } in
    let rec helper ctx return_value = function
      | [] -> return (ctx, return_value)
      | hd :: tl ->
        (match eval_form ctx hd with
         | Ok (ctx, return_value) -> helper ctx return_value tl
         | Error e -> Result.Error e)
    in
    helper ctx VVoid ast
  ;;
end

let parse_and_interpr_prog str =
  let module I = Interpret in
  match parse str with
  | Ok ast -> I.interpr_prog ast
  | Error e -> Error (Format.sprintf "Error <%s> while parsing\n" e)
;;
