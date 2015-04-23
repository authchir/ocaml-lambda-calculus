{
open Printf
}

let letter = ['a'-'z' 'A'-'Z']
let digit = ['0'-'9']
let identifier = letter (letter | digit)*
let whitespace = [' ' '\n']

rule token = parse
  | '\\' { Parser.Lambda }
  | '.' { Parser.Dot }
  | '(' { Parser.LParen }
  | ')' { Parser.RParen }
  | identifier as id { Parser.Identifier id }
  | whitespace {
    token lexbuf
  }
  | eof { Parser.EOF }

{
}
