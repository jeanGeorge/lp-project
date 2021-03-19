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

fun teval (e:expr, p:env) =
    case e of
        (* Var _ => teval  *) (* 1 *)
        (ConI _) _ => IntT (* 2 *)
        | (ConB _) _ => BoolT (* 3 e 4 *)
        | [] _ => ListT [] (* 5 ? *)
        | (List e) p => map (fn x => teval(x, p)) e (* 6 ? *)
        | ESeq (SeqT t) => SeqT t (* 7 *)
        | Let(x, e1, e2) p => teval(e2, (x, teval(e1, p))::p) (* 8 ? *)
        | Letrec (f, argType, x, fType, e1, e2) p => (* 9 ? *)
            let
                val pRec = (f, FunT(argType, fType))
                val e1Type = teval(e1, pRec::(x, argType)::p)
                val e2Type = teval(e2, (pRec::p))
            in
                if e1Type = fType then
                    e2Type
                else
                    raise WrongRetType
            end
        | Anon(type, x, exp) p => FunT(type, teval(exp, (x, type)::p)) (* 10 *)
        (* 11 *)
        | If(e1, e2, e3) p =>  (* 12 *)
            case teval (e1, p) of
                BoolT =>
                    if teval(e1, p) = teval(e3, p) then
                        teval(e2, p)
                    else
                        raise DiffBrTypes
                | _ => raise IfCondNotBool
        (* | Match(e1, l) p =>  (* 13 *) *)
        | Prim1(oper, e1) p => (* 14, 15, 16, 17, 18, 19 *)
            case oper of
                "!" =>
                    if teval(e1, p) = BoolT then
                        BoolT
                    else
                        raise UnknownType
                | "-" =>
                    if teval(e1, p) = IntT then
                        IntT
                    else
                        raise UnknownType
                | "hd" =>
                    if teval(e1, p) = (SeqT t) then
                        (SeqT t)
                    else
                        raise UnknownType
                | "tl" =>
                    if teval(e1, p) = (SeqT t) then
                        (SeqT t)
                    else
                        raise UnknownType
                | "ise" =>
                    if teval(e1, p) = (SeqT t) then
                        BoolT
                    else
                        raise UnknownType
                | "print" => List[]
                | _  => raise UnknownType
        | Prim2(oper, e1, e2) => (* 20, 21, 22, 23, 24 *)
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
                        | (ListT t, SeqT t) => 
                            if t = (ListT t) then
                                SeqT t
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
                (* 25 *)
                | _ => raise UnknownType
        | _ => raise UnknownType

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
