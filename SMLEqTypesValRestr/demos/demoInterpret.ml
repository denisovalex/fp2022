(** Copyright 2022-2023, Anton Kraev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

let () =
  SMLEqTypesValRestr_lib.Interpreter.run_interpreter (Stdio.In_channel.input_all stdin)
;;
