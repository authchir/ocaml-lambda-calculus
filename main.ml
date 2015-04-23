open Core.Std

let () =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.toplevel Lexer.token lexbuf Syntax.empty_context in
  let expr' = Eval.by_value expr in
  if Syntax.is_value expr' then
    print_endline (Syntax.to_string expr ^ " evaluates to " ^ Syntax.to_string expr')
  else
    print_endline (Syntax.to_string expr ^ " does not evaluates to a value")
