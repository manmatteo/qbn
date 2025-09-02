module Ast = Parsing.Ast
module Parse = Parsing.Parse

let cases = [
  ("true", "true");
  ("false", "false");
  ("x", "x");
  ("x'", "x'");
  ("(true, false)", "(true, false)");
  ("(x, (true, false))", "(x, (true, false))");
  ("let x = true in x", "let x = true in x");
  ("let x =\n true in\n (x, x)", "let x = true in (x, x)");

  ("let (x, y) = (true, false) in x", "let (x, y) = (true, false) in x");
  ("let s = sample in s", "let s = sample in s");

  ("case x of true -> let y = x in y | false -> false end",
   "case x of true -> let y = x in y | false -> false end");
]

let run_case (src, expected) =
  let term = Parse.parse_string src in
  let got = Ast.string_of_term term in
  if got <> expected then begin
    Printf.eprintf "Input: %S\nExpected: %S\nGot: %S\n" src expected got;
    exit 1
  end

let () = List.iter run_case cases

let negative_cases = [
  "";                                 (* empty input *)
  "let x = true";                     (* missing 'in' *)
  "let (x y) = (true, false) in x";   (* missing comma in pattern *)
  "(true, false";                     (* unclosed paren *)
  "case x of true -> end";            (* missing RHS term before end *)
  "case x of -> true end";            (* missing LHS value *)
  "case (let x = true in x) of true -> false | false -> true"; (* term as value *)
  "@";                                (* invalid character *)
  "let x = (true, in x";             (* unterminated pair *)
  "case x of true -> y | false -> z"; (* missing end *)
]

let run_negative_case src =
  try
    let _ = Parse.parse_string src in
    Printf.eprintf "Expected parse failure for: %S\n" src;
    exit 1
  with
  | Parsing.Parser.Error
  | Failure _ -> ()

let () = List.iter run_negative_case negative_cases
