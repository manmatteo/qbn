{
open Parser
}

rule token = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r'] { token lexbuf }
  | "let" { LET }
  | "in" { IN }
  | "case" { CASE }
  | "of" { OF }
  | "end" { END }
  | "sample" { SAMPLE }
  | "true" { TRUE }
  | "false" { FALSE }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | ',' { COMMA }
  | '|' { BAR }
  | "->" { ARROW }
  | '=' { EQ }
  | ['A'-'Z' 'a'-'z' '_']['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']* as id { IDENT id }
  | eof { EOF }
  | _ as c { failwith (Printf.sprintf "Unexpected character: %C" c) }
