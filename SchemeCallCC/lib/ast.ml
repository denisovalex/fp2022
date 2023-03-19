(** Copyright 2022-2023, Denisov Alexey and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | Int of int
  | String of string
  | Bool of bool

and datum =
  | DConst of const
  | DList of datum list
  | DAbbr of prefix * datum

and prefix =
  | PQuote
  | PBackquote
  | PComma

and quasiquote =
  | QConst of const
  | QDatum of datum
  | QList of quasiquote list
  | QUnquote of expression

and id = string

and formals =
  | FormalList of id list
  | Formal of id

and expression =
  | Const of const
  | Var of id
  | Quote of datum
  | Quasiquote of quasiquote
  (*| Lambda of formals * definition list * expression list*)
  | If of expression * expression * expression option
  | FuncCall of expression * expression list

and definition = id * expression

and form =
  | Def of definition
  | Expr of expression

and program = form list
