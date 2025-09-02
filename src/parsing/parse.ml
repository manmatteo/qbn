open Ast

let slice s bol cnum =
  let line_start = bol in
  let i = ref line_start in
  while !i < String.length s && s.[!i] <> '\n' do incr i done;
  let line_end = !i in
  let col = cnum - bol + 1 in
  let caret = String.make (max 0 (col - 1)) ' ' ^ "^" in
  (String.sub s line_start (line_end - line_start), caret)

let parse_string (s:string) : term =
  let lexbuf = Lexing.from_string s in
  try
    Parser.program Lexer.token lexbuf
  with
  | Parser.Error ->
      let pos = lexbuf.lex_curr_p in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      let (line_src, caret) = slice s pos.pos_bol pos.pos_cnum in
      let msg = Printf.sprintf "parse error at line %d, column %d\n%s\n%s"
        line col line_src caret in
      raise (Failure msg)
  | Failure m ->
      let pos = lexbuf.lex_curr_p in
      let line = pos.pos_lnum in
      let col = pos.pos_cnum - pos.pos_bol + 1 in
      let (line_src, caret) = slice s pos.pos_bol pos.pos_cnum in
      let msg = Printf.sprintf "%s at line %d, column %d\n%s\n%s"
        m line col line_src caret in
      raise (Failure msg)
