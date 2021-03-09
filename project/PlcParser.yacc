%%

%name PlcParser

%pos int

%term VAR | FUN | FUNREC | ANONFUN | ANONARR | END
    | IF | THEN | ELSE
    | MATCH | WITH
    | NOT | AND | OR
    | HD | TL
    | ISE
    | PRINT
    | PLUS | MINUS | MULTI | DIV
    | EQ | NEQ | LT | LTE | GT | GTE
    | SEMIC | COMMA | COLON | DCOLON | RARROW | PIPE | UNDERLINE | LPAR | RPAR | LSBRAC | RSBRAC | LCBRAC | RCBRAC
    | NAME of string
    | CINT of int | CBOOL of bool
    | NIL | BOOL | INT
    | EOF

%nonterm Prog of expr | Expr of expr | Decl of expr | AtomExpr of expr | AppExpr of expr | Const of expr | Comps of expr | MatchExpr of expr | CondExpr of expr
    | Args of plcVal | Params of plcVal | TypedVar of plcType | Type of plcType | AtomType of plcType | Types of plcType

%right SEMIC RARROW DCOLON

%left ELSE AND EQ NEQ LT LTE PLUS MINUS MULTI DIV LSBRAC

%nonassoc IF NOT HD TL ISE PRINT FUN

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
  | Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
  | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
  | FUNREC NAME Args COLON Type EQ Expr (Letrec(NAME, Args, Type, Expr))

Expr : AtomExpr (AtomExpr)
  | AppExpr (AppExpr)
  | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))

AtomExpr : Const (Const)
  | NAME (Var(NAME))
  | LCBRAC Prog RCBRAC (Prog)
  | LPAR Expr RPAR (Expr)
  | LPAR Comps RPAR (Comps)
  | ANONFUN Args ANONARR Expr END (Anon(Args, Expr))

Comps : Expr COMMA Expr ()
  | Expr COMMA Comps ()

Args : LPAR RPAR ()
  | LPAR Params RPAR (Params)

Params : TypedVar (TypedVar)
  | TypedVar COMMA Params (TypedVar)

TypedVar : Type NAME (Var(NAME))

Type : AtomType (AtomType)
  | LPAR Types RPAR (ListT(Types))
  | LSBRAC Types RSBRAC (SeqT(Types))
  | Type RARROW Type (Type)

Types : Type COMMA Type (ListT(Type1, Type2))
  | Type COMMA Types (Type)

AtomType : NIL ()
  | BOOL (BoolT)
  | INT (IntT)
  | LPAR Type RPAR (Type)

Const : CINT (ConI(CINT))
  | CBOOL (ConB(CBOOL))
  | LPAR RPAR ()
  | LPAR Type LSBRAC RSBRAC RPAR (Type)
  