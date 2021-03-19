(* Plc interpreter main file *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";

use "Parse.sml";
use "PlcInterp.sml";
use "PlcChecker.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

fun tevalAux(exp) = eval exp []
    handle EmptySeq => print("Error: EmptySeq")
    | UnknownType => print("Error: UnknownType")
    | NotEqTypes => print("Error: NotEqTypes")
    | WrongRetType => print("Error: WrongRetType")
    | DiffBrTypes => print("Error: DiffBrTypes")
    | IfCondNotBool => print("Error: IfCondNotBool")
    | NoMatchResults => print("Error: NoMatchResults")
    | MatchResTypeDiff => print("Error: MatchResTypeDiff")
    | MatchCondTypesDiff => print("Error: MatchCondTypesDiff")
    | CallTypeMisM => print("Error: CallTypeMisM")
    | ListOutOfRange => print("Error: ListOutOfRange")
    | OpNonList => print("Error: OpNonList")

fun evalAux(exp) = eval exp []
    handle Impossible => print("Error: Impossible")
    | HDEmptySeq => print("Error: HDEmptySeq")
    | TLEmptySeq => print("Error: TLEmptySeq")
    | ValueNotFoundInMatch => print("Error: ValueNotFoundInMatch")
    | NotAFunc => print("Error: NotAFunc")
    | SymbolNotFound => print("Error: SymbolNotFound")

fun run exp =
    let
        val type = tevalAux(exp)
        val value = evalAux(exp)
    in
        val2string(type) ^ " : " ^ type2string(value)