(* PlcChecker *)

fun teval (e:expr, plcType:env) =
    case e of
        ConI _ => IntT (* regra 2 *)
        | ConB _ => BoolT (* 3 e 4 *)
        | ESeq (SeqT t) => SeqT t (* 7 *)
        | () _ => ListT [] (* 5 *)
        | _ => raise UnknownType

(* exception EmptySeq
exception UnknownType
exception NotEqTypes
exception WrongRetType
exception DiffBrTypes
exception IfCondNotBool
exception NoMatchResults
exception MatchResTypeDiff
exception MatchCondTypesDiff
exception CallTypeMisM
exception NotFunc
exception ListOutOfRange
exception OpNonList *)
