%%

%name PlcParser

%pos int

%term VAR | FUN | FUNREC | ANONFUN | ANONARR | END
    | IF | THEN | ELSE
    | MATCH | WITH
    | NOT | AND | NEG
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

%nonterm Prog of expr
    | Expr of expr
    | Decl of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr
    | MatchExpr of expr
    | CondExpr of expr
    | Args of plcVal
    | Params of plcVal
    | TypedVar of plcType
    | Type of plcType
    | AtomType of plcType
    | Types of plcType

%right SEMIC RARROW DCOLON

%left ELSE AND EQ NEQ LT LTE PLUS MINUS MULTI DIV LSBRAC

%nonassoc IF NOT HD TL ISE PRINT FUN NEG

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
  | Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
  | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
  | FUNREC NAME Args COLON Type EQ Expr SEMIC Prog (Letrec(NAME, Args, Type, Expr, Prog))

Expr : AtomExpr (AtomExpr)
  | AppExpr (AppExpr)
  | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
  | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
  | NOT Expr (Prim1("!", Expr))
  | NEG Expr (Prim1("-", Expr))
  | HD Expr (Prim1("hd", Expr))
  | TL Expr (Prim1("tl", Expr))
  | ISE Expr (Prim1("ise", Expr))
  | PRINT Expr (Prim1("print", Expr))
  | Expr AND Expr (Prim2("&&", Expr1, Expr2))
  | Expr PLUS Expr (Prim2("+", Expr1, Expr2))
  | Expr MINUS Expr (Prim2("-", Expr1, Expr2))
  | Expr MULTI Expr (Prim2("*", Expr1, Expr2))
  | Expr DIV Expr (Prim2("/", Expr1, Expr2))
  | Expr EQ Expr (Prim2("=", Expr1, Expr2))
  | Expr NEQ Expr (Prim2("!=", Expr1, Expr2))
  | Expr LT Expr (Prim2("<", Expr1, Expr2))
  | Expr LTE Expr (Prim2("<=", Expr1, Expr2))
  | Expr DCOLON Expr (Prim2("::", Expr1, Expr2))
  | Expr SEMIC Expr (Prim2(";", Expr1, Expr2))
  | Expr LSBRAC CINT RSBRAC (Item(Expr, CINT))

AtomExpr : Const (Const)
  | NAME (Var(NAME))
  | LCBRAC Prog RCBRAC (Prog)
  | LPAR Expr RPAR (Expr)
  | LPAR Comps RPAR (Comps)
  | ANONFUN Args ANONARR Expr END (Anon(Args, Expr))

Comps : Expr COMMA Expr (list(Expr1::Expr2::[]))
  | Expr COMMA Comps (list(Expr::Comps))

AppExpr : AtomExpr AtomExpr (list(AtomExpr1::AtomExpr2::[]))
  | AppExpr AtomExpr (list(AtomExpr::AppExpr))

Args : LPAR RPAR ()
  | LPAR Params RPAR (Params)

Params : TypedVar (list(TypedVar::[]))
  | TypedVar COMMA Params (list(TypedVar::Params))

TypedVar : Type NAME (Var(NAME))

Type : AtomType (AtomType)
  | LPAR Types RPAR (ListT(Types))
  | LSBRAC Type RSBRAC (SeqT(Type))
  | Type RARROW Type (Type)

Types : Type COMMA Type (list(Type1::Type2::[]))
  | Type COMMA Types (list(Type::Types))

AtomType : NIL ()
  | BOOL (BoolT)
  | INT (IntT)
  | LPAR Type RPAR (Type)

Const : CINT (ConI(CINT))
  | CBOOL (ConB(CBOOL))
  | LPAR RPAR ()
  | LPAR Type LSBRAC RSBRAC RPAR (Type)