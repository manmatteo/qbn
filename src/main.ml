module Ast = Parsing.Ast
module Parse = Parsing.Parse

let read_all ic = really_input_string ic (in_channel_length ic)

let () =
  let input =
    if Array.length Sys.argv > 1 then begin
      let fname = Sys.argv.(1) in
      let ic = open_in_bin fname in
      Fun.protect ~finally:(fun () -> close_in_noerr ic) (fun () -> read_all ic)
    end else
      try read_all stdin with _ -> ""
  in
  try
    let term = Parse.parse_string input in
    print_endline (Ast.string_of_term term)
  with
  | Failure msg ->
      prerr_endline ("Error: " ^ msg); exit 1
