%{
open Core.Std
open Lambda
%}

%token <string> Identifier
%token Lambda Colon Dot LParen RParen
%token Arrow
%token Bool True False If Then Else
%token EOF

%right Arrow

%start toplevel
%type <Lambda.Syntax.context -> Lambda.Term.t> toplevel

%%

toplevel:
  term EOF { fun ctxt -> $1 ctxt }

term: 
  abs { fun ctxt -> $1 ctxt }
| app { fun ctxt -> $1 ctxt }
| If term Then term Else term { fun ctxt -> Term.If ($2 ctxt, $4 ctxt, $6 ctxt) }

abs:
  Lambda Identifier Colon typ Dot term { fun ctxt ->
    Term.Abs ($4 ctxt, $6 (Syntax.add_binding ctxt $2))
  }

typ:
  typ Arrow typ { fun ctxt -> Type.Fun ($1 ctxt, $3 ctxt) }
| Bool { fun ctxt -> Type.Bool }
| LParen typ RParen { fun ctxt -> $2 ctxt }

app:
  var { fun ctxt -> $1 ctxt }
| app var { fun ctxt -> Term.App ($1 ctxt, $2 ctxt) }

var:
  Identifier { fun ctxt -> Term.Var (Syntax.name_to_index ctxt $1) }
| LParen term RParen { fun ctxt -> $2 ctxt }
| True { fun ctxt -> Term.True }
| False { fun ctxt -> Term.False }

%%
