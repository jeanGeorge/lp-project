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
        val tevalResult = type2string(teval e [])
        val evalResult = val2string(eval e [])
    in
        evalResult ^ " : " ^ tevalResult
    end
    handle
        (* PlcChecker.sml *)
        EmptySeq => "Error: EmptySeq"
        | UnknownType => "Error: UnknownType"
        | NotEqTypes => "Error: NotEqTypes"
        | WrongRetType => "Error: WrongRetType"
        | DiffBrTypes => "Error: DiffBrTypes"
        | IfCondNotBool => "Error: IfCondNotBool"
        | NoMatchResults => "Error: NoMatchResults"
        | MatchResTypeDiff => "Error: MatchResTypeDiff"
        | MatchCondTypesDiff => "Error: MatchCondTypesDiff"
        | CallTypeMisM => "Error: CallTypeMisM"
        | ListOutOfRange => "Error: ListOutOfRange"
        | OpNonList => "Error: OpNonList"
        (* PlcInterp.sml *)
        | HDEmptySeq => "Error: HDEmptySeq"
        | TLEmptySeq => "Error: TLEmptySeq"
        | ValueNotFoundInMatch => "Error: ValueNotFoundInMatch"
        | NotAFunc => "Error: NotAFunc"
        (* Environ.sml *)
        | SymbolNotFound => "Error: SymbolNotFound"