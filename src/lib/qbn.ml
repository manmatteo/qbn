open Parsing.Ast

type move = Left | Right
type tape = move list
type pos = term (* Paired with context? *)
let mk_pos (t:term) : pos = t
type path = pos list

let pp_path fmt (p:path) =
  Format.fprintf fmt "[%a]" (Format.pp_print_list
    ~pp_sep:(fun fmt () -> Format.fprintf fmt ", ") pp_term) p

type assignment = VAssign of var * closure | PAssign of var * var * closure
and closure = term * env
and env = assignment list

let get_binding x e =
  List.find_map (function
    | VAssign (y, (u, e')) when x = y -> Some (u, e',None)
    | PAssign (y, _, (v, e')) when x = y -> Some (v, e', Some Left)
    | PAssign (_, z, (v, e')) when x = z -> Some (v, e', Some Right)
    | _ -> None) e

let rec kam tm e tape : path =
  match tm with
  | Let (x, u, t) -> kam t (VAssign (x, (u,e))::e) tape
  | Let2 (x, y, v, t) -> kam t (PAssign (x, y, (Val v,e))::e) tape
  | Val (Var v) ->
    begin match get_binding v e with
    | Some (u, e', dir) ->
      let tape = Option.fold ~none:tape ~some:(fun d -> d::tape) dir in
      kam u e' tape
    | None -> raise (Failure "Unbound variable")
    end
  | Val (Pair (v, w)) ->
    begin match tape with
    | Left::tape -> kam (Val v) e tape
    | Right::tape -> kam (Val w) e tape
    | [] ->
      let _ = kam (Val v) e [] in
      kam (Val w) e []
    end
  | Case (v, _branches) ->
    let p = kam (Val v) e tape in
    (mk_pos tm)::p
  | Val (True | False) -> []
  | Sample -> [mk_pos tm]
