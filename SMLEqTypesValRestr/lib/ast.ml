(** Copyright 2022-2023, Anton Kraev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type id = string [@@deriving show { with_path = false }]

type literal =
  | LChar of char
  | LString of string
  | LInt of int
  | LBool of bool
  | LUnit
[@@deriving show { with_path = false }]

type binary_op =
  | Add (* + *)
  | Sub (* - *)
  | Mult (* * *)
  | Div (* / *)
  | Eq (* = *)
  | NotEq (* <> *)
  | Less (* < *)
  | LessOrEq (* <= *)
  | Greater (* > *)
  | GreaterOrEq (* >= *)
  | And (* andalso *)
  | Or (* orelse *)
[@@deriving show { with_path = false }]

type unary_op =
  | Neg (* ~ *)
  | Not (* not *)
[@@deriving show { with_path = false }]

type expr =
  | ELiteral of literal (* 55 *)
  | EIdentifier of id (* varname *)
  | EUnaryOp of unary_op * expr (* ~10 *)
  | EBinaryOp of binary_op * expr * expr (* 7 + 8 *)
  | ETuple of expr list (* ("first", 2, '3') *)
  | EList of expr list (* [99.3, 83.32] *)
  | EConsList of expr * expr (* 4 :: [5, 6] *)
  | ECaseOf of expr * (expr * expr) list
    (* case x 
                                             of 0 => 'zero' 
                                              | _ => 'not zero' *)
  | ELetIn of expr list * expr
    (* let
                                    val a = 3 
                                    val b = 10
                                  in
                                    a + b
                                  end *)
  | EApplication of expr * expr (* f x *)
  | EValDec of id * expr (* val x = 88 *)
  | EValRecDec of
      id
      * expr (* val rec factorial = fn n => if n <= 1 then 1 else n * factorial (n - 1) *)
  | EArrowFun of id list * expr (* fn x => x + 1 *)
  | EIfThenElse of expr * expr * expr (* if true then 1 else 0 *)
[@@deriving show { with_path = false }]

(* smart constructors for expressions *)
let e_literal x = ELiteral x
let e_identifier x = EIdentifier x
let e_unary_op op x = EUnaryOp (op, x)
let e_binary_op op left right = EBinaryOp (op, left, right)
let e_tuple elements = ETuple elements
let e_list elements = EList elements
let e_cons_list head tail = EConsList (head, tail)
let e_case_of expression cases = ECaseOf (expression, cases)
let e_let_in declarations body = ELetIn (declarations, body)
let e_application func args = EApplication (func, args)
let e_val_dec value_id expression = EValDec (value_id, expression)
let e_val_rec_dec value_id expression = EValRecDec (value_id, expression)
let e_arrow_fun args_id expression = EArrowFun (args_id, expression)
let e_if_then_else cond if_true if_false = EIfThenElse (cond, if_true, if_false)

(* binary operations *)
let badd _ = Add
let bsub _ = Sub
let bmul _ = Mult
let bdiv _ = Div
let beq _ = Eq
let bneq _ = NotEq
let bls _ = Less
let blse _ = LessOrEq
let bgt _ = Greater
let bgte _ = GreaterOrEq
let band _ = And
let bor _ = Or

(* unary operations *)
let uneg _ = Neg
let unot _ = Not
