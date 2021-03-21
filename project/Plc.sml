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

val ExceptionMessageSymbolNotFound = "EXCEPTION: MESSAGE SYMBOL NOT FOUND!"
val ExceptionMessageEmptySeq = "EXCEPTION: INPUT SEQUENCE IS EMPTY!"
val ExceptionMessageUnknownType = "EXCEPTION: UNKNOWN EXCEPTION!"
val ExceptionMessageNotEqTypes = "EXCEPTION: COMPARISON WITH NON EQUAL TYPES!"
val ExceptionMessageWrongRetType = "EXCEPTION: FUNCTION RETURNING WRONG TYPE!"
val ExceptionMessageDiffBrTypes = "EXCEPTION: CONDITION WITH DIFFERENT BRANCH TYPES!";
val ExceptionMessageIfCondNotBool = "EXCEPTION: CONDITION IS NOT A BOOLEAN TYPE!"
val ExceptionMessageNoMatchResults = "EXCEPTION: NO RESULTS FOR MATCH EXPRESSION!"
val ExceptionMessageMatchResTypeDiff = "EXCEPTION: MATCH RETURNING DIFFERENT RESULT TYPES!"
val ExceptionMessageMatchCondTypesDiff = "EXCEPTION: MATCH EXPRESSION TYPE DIFFER FROM CONDITION TYPE!"
val ExceptionMessageCallTypeMisM = "EXCEPTION: WRONG ARGUMENT TYPE ON FUNCTION CALL!"
val ExceptionMessageNotFunc = "EXCEPTION: TRYING TO CALL A NON FUNCTION TYPE! (TYPE CHECKER)"
val ExceptionMessageListOutOfRange = "EXCEPTION: LIST INDEX OUT OF RANGE!"
val ExceptionMessageOpNonList = "EXCEPTION: TRYING TO ACCESS INDEX OF A NON LIST EXPRESSION!"

val ExceptionMessageImpossible = "EXCEPTION: IMPOSSIBLE EXCEPTION!"
val ExceptionMessageHDEmptySeq = "EXCEPTION: TRYING TO ACCESS HEAD OF EMPTY SEQUENCE!"
val ExceptionMessageTLEmptySeq = "EXCEPTION: TRYING TO ACCESS TAIL OF EMPTY SEQUENCE!"
val ExceptionMessageValueNotFoundInMatch = "EXCEPTION: VALUE NOT FOUND IN MATCH EXPRESSION!"
val ExceptionMessageNotAFunc = "EXCEPTION: TRYING TO CALL A NON FUNCTION TYPE! (INTERPRETER)"

fun run e =
    let
        val tevalResult = type2string(teval e [])
        val evalResult = val2string(eval e [])
    in
        evalResult ^ " : " ^ tevalResult
    end
    handle
        (* PlcChecker.sml *)
        EmptySeq => raise EmptySeq
        | UnknownType => raise UnknownType
        | NotEqTypes => raise NotEqTypes
        | WrongRetType => raise WrongRetType
        | DiffBrTypes => raise DiffBrTypes
        | IfCondNotBool => raise IfCondNotBool
        | NoMatchResults => raise NoMatchResults
        | MatchResTypeDiff => raise MatchResTypeDiff
        | MatchCondTypesDiff => raise MatchCondTypesDiff
        | CallTypeMisM => raise CallTypeMisM
        | ListOutOfRange => raise ListOutOfRange
        | OpNonList => raise OpNonList
        (* PlcInterp.sml *)
        | HDEmptySeq => raise HDEmptySeq
        | TLEmptySeq => raise TLEmptySeq
        | ValueNotFoundInMatch => raise ValueNotFoundInMatch
        | NotAFunc => raise NotAFunc
        (* Environ.sml *)
        | SymbolNotFound => raise SymbolNotFound

        (* voltar para: *)
        (* EmptySeq => raise EmptySeq
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
        | SymbolNotFound => "Error: SymbolNotFound" *)