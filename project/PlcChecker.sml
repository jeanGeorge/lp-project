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

(*
1-exception EmptySeq A sequência de entrada não contém nenhum elemento
2-exception UnknownType- É usada nas situações onde nenhuma das específicas se encaixa.
3-exception NotEqTypes- Se os tipos usados numa comparação são diferentes.
4-exception WrongRetType- O tipo de retorno da função não condiz com o corpo da mesma.
5-exception DiffBrTypes- Os tipos da expressões dos possíveis caminhos de um If divergem
6-exception IfCondNotBool- A condição do if não é booleana
7-exception NoMatchResults- Não há resultados para a expressão match
8-exception MatchResTypeDiff- O tipo de algum dos casos em match difere dos demais
9-exception MatchCondTypesDiff- O tipo das opções de match difere do tipo da expressão passada para Match
10-exception CallTypeMisM- Você está passando pra uma chamada de função um tipo diferente do qual ela suporta
11-exception NotFunc- Você está tentando chamar algo que não é uma função.
12-exception ListOutOfRange- Tentativa de acessar um elemento fora dos limites da lista
13-exception OpNonList- Tentativa de acessar um elemento em uma expressão que não é uma lista. 
*)


fun teval (e:expr, p:env) =
    case (e,p) of
        (Var x) p => lookup (p,x) (* 1 *)
        | (ConI _) _ => IntT (* 2 *)
        | (ConB _) _ => BoolT (* 3 e 4 *)
        | (List []) _ => ListT [] (* 5 *)
        | (List l) p => ListT map (fn x => teval(x, p)) l (* 6 *)
        | (ESeq s) _ => (* 7 *)
            case s of SeqT t =>
                SeqT t
            | _ => raise EmptySeq
        | Let(x, e1, e2) p => teval(e2, (x, teval(e1, p))::p) (* 8 *)
        | Letrec(f, argType, arg, fType, e1, e2) p => (* 9 *)
            let
                val pRec = (f, FunT(argType, fType))
                val e1Type = teval(e1, pRec::(arg, argType)::p)
                val e2Type = teval(e2, (pRec::p))
            in
                if e1Type = fType then
                    e2Type
                else
                    raise WrongRetType
            end
        | Anon(type, x, exp) p => FunT(type, teval(exp, (x, type)::p)) (* 10 *)
        | Call(e2, e1) p => (* 11 *)
            case teval(exp2, p) of
                FunT(argType, resultType) =>
                    if teval(exp1, p) = argType then
                        resultType
                    else
                        raise CallTypeMisM
                | _ => raise NotFunc
        | If(e1, e2, e3) p => (* 12 *)
            case teval (e1, p) of
                BoolT =>
                    if teval(e1, p) = teval(e3, p) then
                        teval(e2, p)
                    else
                        raise DiffBrTypes
                | _ => raise IfCondNotBool
        | Match(exp, cases) p =>  (* 13 *)
            let
                val expType = teval(exp, p)
                val firstCaseRespType = teval (#2 (hd cases)) p
                fun checkCases (Match(exp, cases)) (p) =
                    case cases of
                        h::[] =>
                            let in
                                case h of
                                    (SOME e1, e2) =>
                                        if teval(e2, p) = firstCaseRespType then
                                            if initialCond = teval(e1, p) then
                                                teval(e2, p)
                                            else
                                                raise MatchCondTypesDiff
                                        else
                                            raise MatchResTypeDiff
                                    | (NONE, e2) =>
                                        if teval(e2, p) = firstCaseRespType then
                                            firstCaseRespType
                                        else
                                            raise MatchResTypeDiff
                            end
                    | h::tail => 
                        let in
                            case h of
                                (SOME e1, e2) =>
                                    if teval(e2, p) = firstCaseRespType then
                                        if initialCond = teval(e1, p) then
                                            checkCases (Match(exp1, tail)) p
                                        else
                                            raise MatchCondTypesDiff
                                    else
                                        raise MatchResTypeDiff
                            | _ => raise UnknownType
                        end
                    | _ => raise NoMatchResults
            in
                checkCases Match(exp, cases) p
            end
        | Prim1(oper, e1) p => (* 14, 15, 16, 17, 18, 19 *)
            case oper of
                "!" => (* 14 *)
                    if teval(e1, p) = BoolT then
                        BoolT
                    else
                        raise UnknownType
                | "-" => (* 15 *)
                    if teval(e1, p) = IntT then
                        IntT
                    else
                        raise UnknownType
                | "hd" => (* 16 *)
                    case teval(e1, p) of
                        (SeqT t) =>
                            (SeqT t)
                        | _ => raise UnknownType
                | "tl" => (* 17 *)
                    case teval(e1, p) of 
                        (SeqT t) =>
                            (SeqT t)
                        | _ => raise UnknownType
                | "ise" => (* 18 *)
                    case teval(e1, p) of 
                        (SeqT t) =>
                            BoolT
                        | _ => raise UnknownType
                | "print" => List[] (* 19 *)
                | _  => raise UnknownType
        | Prim2(oper, e1, e2) p => (* 20, 21, 22, 23, 24 *)
            case oper of
                "&&" => (* 20 *)
                    if teval(e1, p) = BoolT andalso teval(e2, p) = BoolT then
                        BoolT
                    else UnknownType
                | "::" => (* 21 *)
                    case (teval(e1, p), teval(e2, p)) of
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
                | "+" => (* inicio 22 *)
                    if teval(e1, p) = IntT andalso teval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "-" =>
                    if teval(e1, p) = IntT andalso teval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "*" =>
                    if teval(e1, p) = IntT andalso teval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "/" => (* fim 22 *)
                    if teval(e1, p) = IntT andalso teval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "<" => (* 23 *)
                    if teval(e1, p) = IntT andalso teval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "<=" => (* 23 *)
                    if teval(e1, p) = IntT andalso teval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "=" => (* 24 *)
                     if teval(e1, p) = teval(e2, p) andalso (teval(e1, p) = IntT orelse teval(e1,p) = BoolT orelse teval(e1,p) = ListT orelse teval(e1,p) = SeqT) then
                        BoolT
                    else NotEqTypes
                | "!=" => (* 24 *)
                    if teval(e1, p) = teval(e2, p) andalso (teval(e1, p) = IntT orelse teval(e1,p) = BoolT orelse teval(e1,p) = ListT orelse teval(e1,p) = SeqT) then
                        BoolT
                    else NotEqTypes
                | ";" => teval(e2, p) (* 26 *)
                | _ => raise UnknownType
        | Item(i, exp) p => (* 25 *)
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
                case teval (exp,p) of
                    ListT l => getIndexElement(i, l)
                    | _ => raise OpNonList
            end
        | _ => raise UnknownType