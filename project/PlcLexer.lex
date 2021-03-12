(* Plc Lexer *)

(* User declarations *)

open Tokens
type pos = int
type slvalue = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult = (slvalue, pos)token

fun keyword (s, lpos, rpos) =
    case s of
        "var" => VAR(lpos, rpos)
        | "Bool" => BOOL(lpos, rpos)
        | "else" => ELSE(lpos, rpos)
        | "end" => END(lpos, rpos)
        | "false" => CBOOL(false, lpos, rpos)
        | "fn" => ANONFUN(lpos, rpos)
        | "fun" => FUN(lpos, rpos)
        | "hd" => HD(lpos, rpos)
        | "if" => IF(lpos, rpos)
        | "Int" => INT(lpos, rpos)
        | "ise" => ISE(lpos, rpos)
        | "match" => MATCH(lpos, rpos)
        | "Nil" => NIL(lpos, rpos)
        | "print" => PRINT(lpos, rpos)
        | "rec" => FUNREC(lpos, rpos)
        | "then" => THEN(lpos, rpos)
        | "tl" => TL(lpos, rpos)
        | "true" => CBOOL(true, lpos, rpos)
        | "with" => WITH(lpos, rpos)
        | _ => NAME(s, lpos, rpos)

(* A function to print a message error on the screen. *)
val error = fn x => TextIO.output(TextIO.stdOut, x ^ "\n")
val lineNumber = ref 0

(* Get the current line being read. *)
fun getLineAsString() =
    let
        val lineNum = !lineNumber
    in
        Int.toString lineNum
    end

(* Define what to do when the end of the file is reached. *)
fun eof () = Tokens.EOF(0,0)

fun strToInt s =
    case Int.fromString s of
        SOME i => i
      | NONE => raise Fail ("Could not convert string '" ^ s ^ "' to integer")

(* Initialize the lexer. *)
fun init() = ()
%%
%header (functor PlcLexerFun(structure Tokens: PlcParser_TOKENS));
digit=[0-9];
boolTrue=true;
boolFalse=false;
whitespace=[\ \t];
identifier=[a-zA-Z_][a-zA-Z0-9_]*;


%%

\n => (lineNumber := !lineNumber + 1; lex());
{whitespace}+ => (lex());
{digit}+ => (CINT(strToInt(yytext), yypos, yypos));
{boolTrue} => (CBOOL(true, yypos, yypos));
{boolFalse} => (CBOOL(false, yypos, yypos));
{identifier} => (keyword(yytext, yypos, yypos));
"!" => (NOT(yypos, yypos));
"hd" => (HD(yypos, yypos));
"tl" => (TL(yypos, yypos));
"ise" => (ISE(yypos, yypos));
"print" => (PRINT(yypos, yypos));
"&&" => (AND(yypos, yypos));
"+" => (PLUS(yypos, yypos));
"->" => (RARROW(yypos, yypos));
"-" => (MINUS(yypos, yypos));
"*" => (MULTI(yypos, yypos));
"/" => (DIV(yypos, yypos));
"=" => (EQ(yypos, yypos));
"!=" => (NEQ(yypos, yypos));
"<" => (LT(yypos, yypos));
"<=" => (LTE(yypos, yypos));
"::" => (DCOLON(yypos, yypos));
";" => (SEMIC(yypos, yypos));
"," => (COMMA(yypos, yypos));
"(" => (LPAR(yypos, yypos));
")" => (RPAR(yypos, yypos));
"[" => (LSBRAC(yypos, yypos));
"]" => (RSBRAC(yypos, yypos));
"{" => (LCBRAC(yypos, yypos));
"}" => (RCBRAC(yypos, yypos));
"=>" => (ANONARR(yypos, yypos));
"=" => (EQ(yypos, yypos));
"|" => (PIPE(yypos, yypos));
"_" => (UNDERSCORE(yypos, yypos));
. => (error("\n***Lexer error: bad character ***\n"); raise Fail("Lexer error: bad character " ^ yytext));