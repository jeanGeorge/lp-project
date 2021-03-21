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
        Var x => lookup p x
        | ConI _ => IntT (* 2 *)
        | ConB _ => BoolT (* 3 e 4 *)
        | List [] => ListT [] (* 5 *)
        | List l => (* 6 *)
            let
                val mappedList = map(fn x => teval x p) l
            in
                ListT mappedList
            end
        | ESeq (SeqT x) => SeqT x
        | Let(x, e1, e2) => teval e2 ((x, teval e1 p)::p)
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
        | Anon(t, x, e) => FunT(t, (teval e ((x,t)::p)))
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
                        if (teval e2 p) = (teval e3 p) then
                            (teval e2 p)
                        else
                            raise DiffBrTypes
                    | _ => raise IfCondNotBool
            end
        | Match (exp, cases) =>
            if cases = [] then
                raise NoMatchResults
            else
                let
                    val expType = teval exp p
                    val firstResult = (#2 (hd cases))
                    val firstResultType = teval firstResult p
                    fun readList(h::[]) =
                        let in
                            case h of
                                (SOME e, res) =>
                                    if teval e p <> expType then
                                        raise MatchCondTypesDiff
                                    else
                                        if (teval res p) <> firstResultType then
                                            raise MatchResTypeDiff
                                        else
                                            firstResultType
                                | (NONE, res) =>
                                    if (teval res p) <> firstResultType then
                                        raise MatchResTypeDiff
                                    else
                                        firstResultType
                        end
                    | readList(h::tail) =
                        let in
                            case h of
                                (SOME e, res) =>
                                    if (teval e p) <> expType then
                                        raise MatchCondTypesDiff
                                    else
                                        if (teval res p) <> firstResultType then
                                            raise MatchResTypeDiff
                                        else
                                            readList(tail)
                                | _ => raise UnknownType
                        end
                in
                    readList(cases)
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
                        if e1 <> ESeq(teval e1 p) then
                            let in
                                case (teval e1 p) of
                                    (SeqT t) => t
                                    | _ => raise UnknownType
                            end
                        else
                            raise EmptySeq
                    | "tl" => (* 17 *)
                        let in
                            if e1 <> ESeq(teval e1 p) then
                                case (teval e1 p) of
                                    (SeqT t) => (SeqT t)
                                    | _ => raise UnknownType
                            else
                                raise EmptySeq
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
                            case (teval e1 p, teval e2 p) of
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
                        else
                            raise UnknownType
                    | "-" =>
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else
                            raise UnknownType
                    | "*" =>
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else raise UnknownType
                    | "/" => (* fim 22 *)
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            IntT
                        else
                            raise UnknownType
                    | "<" => (* 23 *)
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            BoolT
                        else
                            raise UnknownType
                    | "<=" => (* 23 *)
                        if (teval e1 p) = IntT andalso (teval e2 p) = IntT then
                            BoolT
                        else
                            raise UnknownType
                    | "=" => (* 24 *)
                        let in
                            case (teval e1 p) of
                                IntT =>
                                    if (teval e2 p) = IntT andalso (teval e1 p) = (teval e2 p) then
                                        BoolT
                                    else
                                        raise NotEqTypes
                                | BoolT =>
                                    if (teval e2 p) = BoolT andalso (teval e1 p) = (teval e2 p) then
                                        BoolT
                                    else
                                        raise NotEqTypes
                                | SeqT t =>
                                    let in
                                        case t of
                                            BoolT => BoolT
                                            | IntT => BoolT
                                            | ListT([]) => BoolT
                                            | _ => raise NotEqTypes
                                    end
                                | ListT([]) =>
                                    if (teval e2 p) = ListT([]) andalso (teval e1 p) = (teval e2 p) then
                                        BoolT
                                    else
                                        raise NotEqTypes
                                | ListT(types) =>
                                    let
                                        val aux = map(
                                            fn(t) =>
                                                case t of
                                                    BoolT => BoolT
                                                    | IntT => IntT
                                                    | ListT([]) => ListT([])
                                                    | _ => raise NotEqTypes
                                        ) types
                                    in
                                        BoolT
                                    end
                                | _ => raise NotEqTypes
                        end
                    | "!=" => (* 24 *)
                        let in
                            case (teval e1 p) of
                                IntT =>
                                    if (teval e2 p) = IntT andalso (teval e1 p) = (teval e2 p) then
                                        BoolT
                                    else
                                        raise NotEqTypes
                                | BoolT =>
                                    if (teval e2 p) = BoolT andalso (teval e1 p) = (teval e2 p) then
                                        BoolT
                                    else
                                        raise NotEqTypes
                                | SeqT t =>
                                    let in
                                        case t of
                                            BoolT => BoolT
                                            | IntT => BoolT
                                            | ListT([]) => BoolT
                                            | _ => raise NotEqTypes
                                    end
                                | ListT([]) =>
                                    if (teval e2 p) = ListT([]) andalso (teval e1 p) = (teval e2 p) then
                                        BoolT
                                    else
                                        raise NotEqTypes
                                | ListT(types) =>
                                    let
                                        val aux = map(
                                            fn(t) =>
                                                case t of
                                                    BoolT => BoolT
                                                    | IntT => IntT
                                                    | ListT([]) => ListT([])
                                                    | _ => raise NotEqTypes
                                        ) types
                                    in
                                        BoolT
                                    end
                                | _ => raise NotEqTypes
                        end
                    | ";" => (teval e2 p) (* 26 *)
                    | _ =>
                        raise UnknownType
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