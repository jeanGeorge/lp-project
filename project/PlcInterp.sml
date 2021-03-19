(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr, p:env) =
    case e of
        (Var v) p => lookup(p, v) (* 1 *)
        | (ConI i) _ => IntV i (* 2 *)
        | (ConB b) _ => BoolV b (* 3 e 4 *)
        | List [] _ => ListV [] (* 5 ? *)
        | (List e) p => ListV map (fn x => eval(x, p)) e (* 6 ? *)
        | (ESeq e) _ => SeqV [] (* 7 *)
        | Let(x, e1, e2) p => eval(e2, (x, eval(e1, p))::p) (* 8 ? *)
        | Letrec (f, argType, x, fType, e1, e2) p => eval(e2, (f, Clos(f, x, e1, p))) (* 9 ? *)
        | Anon(type, x, exp) p => Clos("", x, exp, p) (* 10 *) (* We need to check if var can be found in the env of Anon *)
        (* 11 *)
        | If(e1, e2, e3) p =>  (* 12 *)
            case eval (e1, p) of
                BoolV true => eval(e2, p)
                | BoolV false => eval(e3, p)
                | _ => raise Impossible
        (* | Match(e1, l) p =>  (* 13 *) *)
        | Prim1(oper, e1) p => (* 14, 15, 16, 17, 18, 19 faltando *)
            case oper of
                "!" =>
                    if eval(e1, p) = BoolT then
                        BoolT
                    else
                        raise UnknownType
                | "-" =>
                    if eval(e1, p) = IntT then
                        IntT
                    else
                        raise UnknownType
                | "hd" =>
                    if eval(e1, p) = (SeqT t) then
                        (SeqT t)
                    else
                        raise UnknownType
                | "tl" =>
                    if eval(e1, p) = (SeqT t) then
                        (SeqT t)
                    else
                        raise UnknownType
                | "ise" =>
                    if eval(e1, p) = (SeqT t) then
                        BoolT
                    else
                        raise UnknownType
                | "print" => List[]
                | _  => raise UnknownType
        | Prim2(oper, e1, e2) => (* 20, 21, 22, 23, 24 faltando *)
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
