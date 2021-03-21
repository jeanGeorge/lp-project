(* Infrastructure to run the Plc Front-End *)

CM.make("$/basis.cm");
CM.make("$/ml-yacc-lib.cm");

use "Environ.sml";
use "Absyn.sml";
use "PlcParserAux.sml";
use "PlcParser.yacc.sig";
use "PlcParser.yacc.sml";
use "PlcLexer.lex.sml";
use "Parse.sml";
use "PlcChecker.sml";
use "PlcInterp.sml";
use "Plc.sml";

Control.Print.printLength := 1000;
Control.Print.printDepth  := 1000;
Control.Print.stringDepth := 1000;

open PlcFrontEnd;

use "testPlcCases.sml";

fun testCases (sourceCode:string, expected:string) : string =
  let
    val expr = fromString(sourceCode);
    val observed = run(expr)
  in
    if (observed = expected) then "V" else sourceCode
  end
  handle EmptySeq => if (ExceptionMessageEmptySeq = expected) then "V" else sourceCode
    | UnknownType => if (ExceptionMessageUnknownType = expected) then "V" else sourceCode
    | NotEqTypes => if (ExceptionMessageNotEqTypes = expected) then "V" else sourceCode
    | WrongRetType => if (ExceptionMessageWrongRetType = expected) then "V" else sourceCode
    | DiffBrTypes => if (ExceptionMessageDiffBrTypes = expected) then "V" else sourceCode
    | IfCondNotBool => if (ExceptionMessageIfCondNotBool = expected) then "V" else sourceCode
    | NoMatchResults => if (ExceptionMessageNoMatchResults = expected) then "V" else sourceCode
    | MatchResTypeDiff => if (ExceptionMessageMatchResTypeDiff = expected) then "V" else sourceCode
    | MatchCondTypesDiff => if (ExceptionMessageMatchCondTypesDiff = expected) then "V" else sourceCode
    | CallTypeMisM => if (ExceptionMessageCallTypeMisM = expected) then "V" else sourceCode
    | NotFunc => if (ExceptionMessageNotFunc = expected) then "V" else sourceCode
    | ListOutOfRange => if (ExceptionMessageListOutOfRange = expected) then "V" else sourceCode
    | OpNonList => if (ExceptionMessageOpNonList = expected) then "V" else sourceCode
    | Impossible => if (ExceptionMessageImpossible = expected) then "V" else sourceCode
    | HDEmptySeq => if (ExceptionMessageHDEmptySeq = expected) then "V" else sourceCode
    | TLEmptySeq => if (ExceptionMessageTLEmptySeq = expected) then "V" else sourceCode
    | ValueNotFoundInMatch => if (ExceptionMessageValueNotFoundInMatch = expected) then "V" else sourceCode
    | NotAFunc => if (ExceptionMessageNotAFunc = expected) then "V" else sourceCode

val results = map (fn (s,e) => testCases(s, e)) cases;

fun isAllTestsPassed [] = true
  | isAllTestsPassed (h::t) = (h="V") andalso (isAllTestsPassed t);

val isPlcCorrect = isAllTestsPassed(results);
