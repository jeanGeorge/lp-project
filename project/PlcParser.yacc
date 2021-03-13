%%

%name PlcParser

%pos int

%term VAR | FUN | REC | ANONFUN | ANONARR | END
    | IF | THEN | ELSE
    | MATCH | WITH
    | NOT | AND
    | HD | TL
    | ISE
    | PRINT
    | PLUS | MINUS | MULTI | DIV
    | EQ | NEQ | LT | LTE
    | SEMIC | COMMA | COLON | DCOLON | RARROW | PIPE | LPAR | RPAR | LSBRAC | RSBRAC | LCBRAC | RCBRAC
    | NIL | BOOL | INT
    | TRUE of bool | FALSE of bool
    | UNDERSCORE
    | NAME of string
    | CINT of int
    | EOF

%nonterm Prog of expr
    | Expr of expr
    | Decl of expr
    | AtomExpr of expr
    | AppExpr of expr
    | Const of expr
    | Comps of expr list
    | MatchExpr of (expr option * expr) list
    | CondExpr of expr option
    | Args of (plcType * string) list
    | Params of (plcType * string) list
    | TypedVar of plcType * string
    | Type of plcType
    | AtomType of plcType
    | Types of plcType list
    | Nat of int
    | Name of string

%right SEMIC RARROW
%nonassoc IF
%left ELSE
%left AND
%left EQ NEQ
%left LT LTE
%right DCOLON
%left PLUS MINUS
%left MULTI DIV
%nonassoc NOT HD TL ISE PRINT
%left LSBRAC

%eop EOF

%noshift EOF

%start Prog

%%

Prog : Expr (Expr)
  | Decl (Decl)

Decl : VAR NAME EQ Expr SEMIC Prog (Let(NAME, Expr, Prog))
  | FUN NAME Args EQ Expr SEMIC Prog (Let(NAME, makeAnon(Args, Expr), Prog))
  | FUN REC NAME Args COLON Type EQ Expr SEMIC Prog (makeFun(NAME, Args, Type, Expr, Prog))

Expr : AtomExpr (AtomExpr)
  | AppExpr (AppExpr)
  | IF Expr THEN Expr ELSE Expr (If(Expr1, Expr2, Expr3))
  | MATCH Expr WITH MatchExpr (Match(Expr, MatchExpr))
  | NOT Expr (Prim1("!", Expr))
  | MINUS Expr (Prim1("-", Expr))
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
  | Expr LSBRAC Nat RSBRAC (Item(Nat, Expr))

AtomExpr : Const (Const)
  | NAME (Var(NAME))
  | LCBRAC Prog RCBRAC (Prog)
  | LPAR Expr RPAR (Expr)
  | LPAR Comps RPAR (List(Comps))
  | ANONFUN Args ANONARR Expr END (makeAnon(Args, Expr))

AppExpr : AtomExpr AtomExpr (Call(AtomExpr1, AtomExpr2))
  | AppExpr AtomExpr (Call(AtomExpr, AppExpr))

Const : TRUE (ConB(TRUE))
  | FALSE (ConB(FALSE))
  | Nat (ConI(Nat))
  | LPAR RPAR (List([]))
  | LPAR Type LSBRAC RSBRAC RPAR (ESeq(Type))

Comps : Expr COMMA Expr (Expr1::Expr2::[])
  | Expr COMMA Comps (Expr::Comps)

MatchExpr : END ([])
  | PIPE CondExpr RARROW Expr MatchExpr ((CondExpr, Expr)::MatchExpr)

CondExpr : Expr (SOME(Expr))
  | UNDERSCORE (NONE)

Args : LPAR RPAR ([])
  | LPAR Params RPAR (Params)

Params : TypedVar (TypedVar::[])
  | TypedVar COMMA Params (TypedVar::Params)

TypedVar : Type NAME (Type, NAME)

Type : AtomType (AtomType)
  | LPAR Types RPAR (ListT(Types))
  | LSBRAC Type RSBRAC (SeqT(Type))
  | Type RARROW Type (FunT(Type1, Type2))

AtomType : NIL (ListT([]))
  | BOOL (BoolT)
  | INT (IntT)
  | LPAR Type RPAR (Type)

Types : Type COMMA Type (Type1::Type2::[])
  | Type COMMA Types (Type::Types)

Nat: CINT(CINT)