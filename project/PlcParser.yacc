%%

%name PlcParser

%pos int

%term VAR | FUN | FUNREC | ANONFUN
    | IF | THEN | ELSE
    | MATCH | WITH
    | NOT | AND | OR
    | HD | TL
    | ISE
    | PRINT
    | PLUS | MINUS | MULTI | DIV
    | EQ | NEQ | LT | LTE | GT | GTE
    | SEMIC | COMMA | DCOLON | RARROW | PIPE | UNDERLINE | LPAR | RPAR | LSBRAC | RSBRAC | LCBRAC | RCBRAC
    | NAME of string
    | CONI of int | CONB of bool
    | NIL

%nonterm Prog of expr | Expr of expr | Decl of expr | AtomExpr of expr | AppExpr of expr | Const of expr | Comps of expr | MatchExpr of expr | CondExpr of expr
    | Args of plcVal | Params of plcVal

%right SEMIC RARROW DCOLON

%left ELSE AND EQ NEQ LT LTE PLUS MINUS MULTI DIV LSBRAC

%nonassoc NOT HD TL ISE PRINT FUN

%eop EOF

%noshift EOF

%start Prog

%%
