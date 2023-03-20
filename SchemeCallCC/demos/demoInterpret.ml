(** Copyright 2022-2023, Denisov Alexey and contributors *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

open Scheme_callcc_lib.Interpreter

let () =
  let input = Stdio.In_channel.input_all Caml.stdin in
  match parse_and_interpr_prog input with
  | Ok (_, return_value) -> Printf.printf "%s" (Interpret.show_value return_value)
  | Error e -> Printf.printf "%s" e
;;
