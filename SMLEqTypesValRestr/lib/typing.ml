(** Copyright 2022-2023, Anton Kraev *)

(** SPDX-License-Identifier: LGPL-3.0-or-later *)

type type_variable_number = int
type identifier = string

type ground_type =
  | Char
  | String
  | Int
  | Bool
  | Unit
[@@deriving show { with_path = false }]

type typ =
  | TVar of type_variable_number (** 'a *)
  | TEqualityVar of type_variable_number (** ''a *)
  | TArr of typ * typ (** string -> int *)
  | TTuple of typ list (** int * int *)
  | TList of typ (** 'a list *)
  | TGround of ground_type (** int *)

(* Ground types *)
let char_typ = TGround Char
let string_typ = TGround String
let int_typ = TGround Int
let unit_typ = TGround Unit
let bool_typ = TGround Bool

(* Smart constructors for types *)
let var_t n = TVar n
let var_eq_t n = TEqualityVar n
let arrow_t left_type right_type = TArr (left_type, right_type)
let tuple_t type_list = TTuple type_list
let list_t typ = TList typ

type scheme = (type_variable_number, Base.Int.comparator_witness) Base.Set.t * typ

type error =
  [ `OccursCheck (** Occurs check fail *)
  | `NoVariable of identifier (** Use of undefined variable *)
  | `UnificationFailed of typ * typ (** Expression of a different type was expected *)
  | `Unreachable
    (** Unreachable code. If this error is thrown then there is a bug in parser *)
  ]

let rec pp_type fmt typ =
  let open Format in
  let arrow_format = function
    | TArr _ -> format_of_string "(%a)"
    | _ -> format_of_string "%a"
  in
  match typ with
  | TGround x ->
    (match x with
     | Int -> fprintf fmt "int"
     | String -> fprintf fmt "string"
     | Char -> fprintf fmt "char"
     | Bool -> fprintf fmt "bool"
     | Unit -> fprintf fmt "unit")
  | TTuple value_list ->
    fprintf
      fmt
      "%a"
      (pp_print_list
         ~pp_sep:(fun _ _ -> fprintf fmt " * ")
         (fun fmt typ -> pp_type fmt typ))
      value_list
  | TList typ -> fprintf fmt (arrow_format typ ^^ " list") pp_type typ
  | TArr (typ_left, typ_right) ->
    fprintf fmt (arrow_format typ_left ^^ " -> %a") pp_type typ_left pp_type typ_right
  | TVar var -> fprintf fmt "%s" @@ "'" ^ Char.escaped (Stdlib.Char.chr (var + 97))
  | TEqualityVar var ->
    fprintf fmt "%s" @@ "''" ^ Char.escaped (Stdlib.Char.chr (var + 97))
;;

let print_typ typ =
  let s = Format.asprintf "%a" pp_type typ in
  Format.printf "%s\n" s
;;

let pp_error fmt (err : error) =
  let open Format in
  match err with
  | `OccursCheck -> fprintf fmt "Occurs check failed.\n"
  | `NoVariable identifier -> fprintf fmt "No such variable: %s" identifier
  | `UnificationFailed (t1, t2) ->
    fprintf fmt "Unification failed: type of the expression is ";
    pp_type fmt t1;
    fprintf fmt " but expected type was ";
    pp_type fmt t2
  | `Unreachable -> fprintf fmt "Not reachable."
;;

let print_type_error error =
  let s = Format.asprintf "%a" pp_error error in
  Format.printf "%s\n" s
;;
