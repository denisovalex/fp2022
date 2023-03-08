(** Copyright 2022-2023, Anton Kraev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

val run_inference : Ast.expr -> (Typing.typ, Typing.error) result
val parse_and_inference : string -> unit
