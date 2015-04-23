%{
open Core.Std
%}

%token <string> Identifier
%token Lambda Dot LParen RParen
%token EOF

%start toplevel
%type <Syntax.context -> Syntax.t> toplevel

%%

toplevel:
  term EOF { fun ctxt -> $1 ctxt }

term: 
  abs { fun ctxt -> $1 ctxt }
| app { fun ctxt -> $1 ctxt }

abs:
  Lambda Identifier Dot term { fun ctxt ->
    Syntax.Abs ($4 (Syntax.add_binding ctxt $2))
  }

app:
  var { fun ctxt -> $1 ctxt }
| app var { fun ctxt -> Syntax.App ($1 ctxt, $2 ctxt) }

var:
  Identifier { fun ctxt -> Syntax.Var (Syntax.name_to_index ctxt $1) }
| LParen term RParen { fun ctxt -> $2 ctxt }

%%
