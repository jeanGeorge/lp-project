(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr, p:env) =
    case (e, p) of
        (Var x) p => lookup(p, x) (* 1 *)
        | (ConI x) _ => IntV x (* 2 *)
        | (ConB x) _ => BoolV x (* 3 e 4 *)
        | (List []) _ => ListV [] (* 5 *)
        | (List l) p => ListV map((fn x => eval(x, p)), l) (* 6 *)
        | (ESeq x) _ => SeqV [] (* 7 *)
        | Let(x, e1, e2) p => eval(e2, (x, eval(e1, p))::p) (* 8 *)
        | Letrec (f, argType, x, fType, e1, e2) p => eval(e2, (f, Clos(f, x, e1, p))) (* 9 *)
        | Anon(type, x, exp) p => Clos("", x, exp, p) (* 10 *) (* We need to check if var can be found in the env of Anon *)
        (*11 faltando -> call *)
        | If(e1, e2, e3) p =>  (* 12 *)
            case eval (e1, p) of
                BoolV true => eval(e2, p)
                | BoolV false => eval(e3, p)
                | _ => raise Impossible
        (* 13 faltando -> match *)
        | Prim1(oper, e1) p => (* 14, 15, 16, 17, 18, 19 validar hd , tl e print *)
            case eval(e1, p) of
                IntV x =>
                    case oper of
                        "-" => IntV(~x)
                        | "print" =>
                            let
                                val printVariable = print(val2string(BoolV x) ^ "\n")
                            in
                                ListV []
                            end
                        | _ => raise Impossible
                | BoolV x =>
                    case oper of
                        "!" => BoolV(not x)
                        | "print" =>
                            let
                                val printVariable = print(val2string(BoolV x) ^ "\n")
                            in
                                ListV []
                            end
                        | _ => raise Impossible
                | SeqV x =>
                    case oper of
                        "hd" => hd x handle Empty => HDEmptySeq (* validar *)
                        | "tl" => tl x handle Empty => TLEmptySeq (* validar *)
                        | "ise" => case x of
                            [] => BoolV true
                            | _ => BoolV false
                        | "print" =>
                            let
                                val printVariable = print(val2string(BoolV x) ^ "\n")
                            in
                                ListV []
                            end
                        | _ => raise Impossible
                | ListV x =>
                    case oper of
                        "print" =>
                            let
                                val printVariable = print(val2string(BoolV x) ^ "\n")
                            in
                                ListV []
                            end
                        | _ => raise Impossible
                | _ => raise Impossible
        | Prim2(oper, e1, e2) p => (* 20, 21, 22, 23, 24 faltando *)
            case oper of
                "&&" => (* 20 *)
                    if eval(e1, p) = BoolT andalso eval(e2, p) = BoolT then
                        BoolT
                    else UnknownType
                | "::" => (* 21 *)
                    case (eval(e1, p), eval(e2, p)) of
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
                    if eval(e1, p) = IntT andalso eval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "-" =>
                    if eval(e1, p) = IntT andalso eval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "*" =>
                    if eval(e1, p) = IntT andalso eval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "/" => (* fim 22 *)
                    if eval(e1, p) = IntT andalso eval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "<" => (* 23 *)
                    if eval(e1, p) = IntT andalso eval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "<=" => (* 23 *)
                    if eval(e1, p) = IntT andalso eval(e2, p) = IntT then
                        IntT
                    else UnknownType
                | "=" => (* 24 *)
                     if eval(e1, p) = eval(e2, p) andalso (eval(e1, p) = IntT orelse eval(e1,p) = BoolT orelse eval(e1,p) = ListT orelse eval(e1,p) = SeqT) then
                        BoolT
                    else NotEqTypes
                | "!=" => (* 24 *)
                    if eval(e1, p) = eval(e2, p) andalso (eval(e1, p) = IntT orelse eval(e1,p) = BoolT orelse eval(e1,p) = ListT orelse eval(e1,p) = SeqT) then
                        BoolT
                    else NotEqTypes
                | ";" => eval(e2, p) (* 26 *)
                (* 25 *)
                | _ => raise UnknownType
        | _ => raise UnknownType
