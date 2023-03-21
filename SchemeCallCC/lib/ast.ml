(** Copyright 2022-2023, Denisov Alexey type contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type const =
  | Int of int
  | String of string
  | Bool of bool

type id = string

type formals =
  | FormalList of id list
  | Formal of id

type prefix =
  | PQuote
  | PBackquote
  | PComma

type datum =
  | DConst of const
  | DList of datum list
  | DAbbr of prefix * datum

type quasiquote =
  | QConst of const
  | QDatum of datum
  | QList of quasiquote list
  | QUnquote of expression

and expression =
  | Const of const
  | Var of id
  | Quote of datum
  | Quasiquote of quasiquote
  (*| Lambda of formals * definition list * expression list*)
  | If of expression * expression * expression option
  | FuncCall of expression * expression list

and definition = id * expression

type form =
  | Def of definition
  | Expr of expression

type program = form list
