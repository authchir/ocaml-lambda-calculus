open Core.Std
open Lambda
open Printf

let () =
  let lexbuf = Lexing.from_channel stdin in
  let expr = Parser.toplevel Lexer.token lexbuf Syntax.empty_context in
  match Type.of_term [] expr with
  | None -> print_endline "Expression does not typecheck"
  | Some ty ->
    let expr' = Eval.by_value expr in
    if Term.is_value expr' then
      printf "%s\nevaluates to\n%s\nof type\n%s\n"
        (Term.to_string expr)
        (Term.to_string expr')
        (Type.to_string ty)
    else
      printf "%s does not evaluates to a value" (Term.to_string expr)
