type ty = B | Prod of ty * ty
type var = string

type value =
    Var of var
  | True | False
  | Pair of value * value

type term =
    Val of value
  | Let of var * term * term
  | Let2 of var * var * value * term
  | Sample
  | Case of value * (value * term) list

(* Pretty-printers for debugging / CLI output *)
open Format

let rec pp_value fmt = function
  | True -> pp_print_string fmt "true"
  | False -> pp_print_string fmt "false"
  | Var v -> pp_print_string fmt v
  | Pair (v, w) ->
      pp_print_char fmt '(';
      pp_value fmt v;
      pp_print_string fmt ", ";
      pp_value fmt w;
      pp_print_char fmt ')'

let rec pp_term fmt = function
  | Val v -> pp_value fmt v
  | Sample -> pp_print_string fmt "sample"
  | Let (x, u, t) ->
      fprintf fmt "let %s = %a in %a" x pp_term u pp_term t
  | Let2 (x, y, v, t) ->
      fprintf fmt "let (%s, %s) = %a in %a" x y pp_value v pp_term t
  | Case (v, branches) ->
      pp_print_string fmt "case ";
      pp_value fmt v;
      pp_print_string fmt " of ";
      let pp_branch fmt (pat, tm) =
        fprintf fmt "%a -> %a" pp_value pat pp_term tm
      in
      let rec pp_branches fmt = function
        | [] -> ()
        | [b] -> pp_branch fmt b
        | b :: bs -> fprintf fmt "%a | %a" pp_branch b pp_branches bs
      in
      pp_branches fmt branches;
      pp_print_string fmt " end"

let string_of_term t = asprintf "%a" pp_term t
