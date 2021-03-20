(* PlcChecker *)

exception EmptySeq
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
exception OpNonList

fun teval (e:expr) (p:plcType env) : plcType =
    case e of
        (Var x) => lookup p x (* 1 *)
        | (ConI _) => IntT (* 2 *)
        | (ConB _) => BoolT (* 3 e 4 *)
        | (List []) => ListT [] (* 5 *)
        | (List l) => (* 6 *)
            let
                val mappedList = map(fn x => teval x p) l
            in
                ListT mappedList
            end
        | (ESeq (SeqT x)) => SeqT x (* 7 *)
        | (ESeq _) => raise EmptySeq
        | (Let(x, e1, e2)) => teval e2 ((x, teval e1 p) ::p ) (* 8 *)
        | Letrec(f, argType, arg, fType, e1, e2) => (* 9 *)
            let
                val e1Type = teval e1 ((f, FunT(argType, fType))::(arg, argType)::p)
                val e2Type = teval e2 ((f, FunT(argType, fType))::p)
            in
                if e1Type = fType then
                    e2Type
                else
                    raise WrongRetType
            end
        | Call(e2, e1) => (* 11 *)
            let in
                case (teval e2 p) of
                    FunT(argType, resultType) =>
                        if (teval e1 p) = argType then
                            resultType
                        else
                            raise CallTypeMisM
                    | _ => raise NotFunc
            end
        | If(e1, e2, e3) => (* 12 *)
            let in
                case (teval e1 p) of
                    BoolT =>
                        if (teval e1 p) = teval e3 p then
                            (teval e2 p)
                        else
                            raise DiffBrTypes
                    | _ => raise IfCondNotBool
            end
        | Match(exp, cases) =>
            let
                val expType = teval exp p
                val firstCaseRespType = teval (#2 (hd cases)) p
                fun checkCases (Match(exp, cases)) (p) =
                    case cases of
                        h::[] =>
                            let in
                                case h of
                                    (SOME e1, e2) =>
                                        if (teval e2 p) = firstCaseRespType then
                                            if (teval e1 p) = (teval e1 p) then
                                                (teval e2 p)
                                            else
                                                raise MatchCondTypesDiff
                                        else
                                            raise MatchResTypeDiff
                                    | (NONE, e2) =>
                                        if (teval e2 p) = firstCaseRespType then
                                            firstCaseRespType
                                        else
                                            raise MatchResTypeDiff
                            end
                    | h::tail =>
                        let in
                            case h of
                                (SOME e1, e2) =>
                                    if (teval e2 p) = firstCaseRespType then
                                        if (teval e1 p) = (teval e1 p) then
                                            checkCases (Match(e1, tail)) p
                                        else
                                            raise MatchCondTypesDiff
                                    else
                                        raise MatchResTypeDiff
                            | _ => raise UnknownType
                        end
                    | _ => raise NoMatchResults
            in
                checkCases (Match(exp, cases)) p
            end
        | Prim1(oper, e1) => (* 14, 15, 16, 17, 18, 19 *)
            let in
                case oper of
                    "!" => (* 14 *)
                        if (teval e1 p) = BoolT then
                            BoolT
                        else raise UnknownType
                    | "-" => (* 15 *)
                        if (teval e1 p) = IntT then
                            IntT
                        else raise UnknownType
                    | "hd" => (* 16 *)
                        let in
                            case (teval e1 p) of
                                (SeqT t) => t
                                | _ => raise UnknownType
                        end
                    | "tl" => (* 17 *)
                        let in
                            case (teval e1 p) of
                                (SeqT t) => (SeqT t)
                                | _ => raise UnknownType
                        end
                    | "ise" => (* 18 *)
                        let in
                            case (teval e1 p) of
                                (SeqT t) => BoolT
                                | _ => raise UnknownType
                        end
                    | "print" => ListT[] (* 19 *)
                    | _  => raise UnknownType
            end
        | Prim2(oper, e1, e2) => (* 20, 21, 22, 23, 24 *)
            let in
                case oper of
                    "&&" => (* 20 *)
                        if (teval e1 p) = BoolT andalso (teval e2 p) = BoolT then
                            BoolT
                        else raise UnknownType
                    | "::" => (* 21 *)
                        let in
                            case (teval e1 p, (teval e2 p)) of
                                (IntT, ListT []) =>
                                    SeqT IntT
                                | (IntT, SeqT t) =>
                                    if t = IntT then
                                        SeqT t
                                    else
                                        raise NotEqTypes
                                | (BoolT, ListT []) =>
                                    SeqT BoolT
                                | (BoolT, SeqT t) =>
                                    if t = BoolT then
                                        SeqT BoolT
                                    else
                                        raise NotEqTypes
                                | (ListT t, ListT []) => SeqT(ListT t)
                                | (ListT t, SeqT s) =>
                                    if s = (ListT t) then
                                        SeqT s
                                    else
                                        raise NotEqTypes
                                | _ => raise UnknownType
                            end
                    | "+" => (* inicio 22 *)
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else raise UnknownType
                    | "-" =>
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else raise UnknownType
                    | "*" =>
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else raise UnknownType
                    | "/" => (* fim 22 *)
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else raise UnknownType
                    | "<" => (* 23 *)
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else raise UnknownType
                    | "<=" => (* 23 *)
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else raise UnknownType
                    | "=" => (* 24 *)
                        if (teval e1 p) = (teval e2 p) andalso ((teval e1 p) = IntT orelse (teval e1 p) = BoolT) then
                            BoolT
                        else raise NotEqTypes
                    | "!=" => (* 24 *)
                        if (teval e1 p) = (teval e2 p) andalso ((teval e1 p) = IntT orelse (teval e1 p) = BoolT) then
                            BoolT
                        else raise NotEqTypes
                    | ";" => (teval e2 p) (* 26 *)
                    | _ => raise UnknownType
            end
        | Item(i, exp) => (* 25 *)
            let
                fun getIndexElement(i, []) = raise ListOutOfRange
                    | getIndexElement(i, (h::[])) =
                        if i = 1 then
                            h
                        else
                            raise ListOutOfRange
                    | getIndexElement(i, (h::tail)) =
                        if i = 1 then
                            h else
                        getIndexElement(i - 1, tail)
            in
                case teval exp p of
                    ListT l => getIndexElement(i, l)
                    | _ => raise OpNonList
            end
        | _ => raise UnknownType