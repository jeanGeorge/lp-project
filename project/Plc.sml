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

fun run e =
    let
        val tevalResult = val2string(teval e [])
        val evalResult = type2string(eval e [])
    in
        evalResult ^ " : " ^ tevalResult
    end
    handle
        (* PlcChecker.sml *)
        EmptySeq => print("Error: EmptySeq")
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
        (* PlcInterp.sml *)
        | HDEmptySeq => print("Error: HDEmptySeq")
        | TLEmptySeq => print("Error: TLEmptySeq")
        | ValueNotFoundInMatch => print("Error: ValueNotFoundInMatch")
        | NotAFunc => print("Error: NotAFunc")
        (* Environ.sml *)
        | SymbolNotFound => print("Error: SymbolNotFound")