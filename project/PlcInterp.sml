(* PlcInterp *)

exception Impossible
exception HDEmptySeq
exception TLEmptySeq
exception ValueNotFoundInMatch
exception NotAFunc

fun eval (e:expr) (p:plcVal env) : plcVal =
    case e of
        Var x => lookup p x (* 1 *)
        | ConI x => IntV x (* 2 *)
        | ConB x => BoolV x (* 3 e 4 *)
        | List [] => ListV [] (* 5 *)
        | List l => (* 6 *)
            let
                val mappedList = map(fn x => eval x p) l
            in
                ListV mappedList
            end
        | ESeq x => SeqV [] (* 7 *)
        | Let(x, e1, e2) => eval e2 ((x, eval e1 p) :: p)
        | Letrec(f, argType, x, fType, e1, e2) =>  eval e2 ((f, Clos(f, x, e1, p)) :: p)
        | Anon(t, x, exp) => Clos ("", x, exp, p)
        | Call(e1, e2) =>
            let
                fun aux (List((h::[]))) = [eval h p]
                    | aux (List(h::tail)) = [eval h p] @ aux(List tail)
                    | aux (exp) = [eval exp p]
            in
                case eval e1 p of
                    Clos(name, x, exp, cEnv) =>
                        eval exp ((x, (eval e2 ([("$list", ListV (aux e2))] @ p)))::(name, eval e1 p)::cEnv) (*testar*)
                    | _ => raise NotAFunc
            end
        | If(e1, e2, e3) =>  (* 12 *)
            let in
                case eval e1 p of
                    BoolV true => eval e2 p
                    | BoolV false => eval e3 p
                    | _ => raise Impossible
            end
        | Match (e1, matchList) =>
            let
                fun checkCases (x, h::[]) p =
                    let in
                        case h of
                            (SOME e2, e3) =>
                                if x = eval e2 p then
                                    e3
                                else
                                    raise ValueNotFoundInMatch
                            | (NONE, e3) =>
                                e3
                    end
                    | checkCases (x, h::tail) p =
                        let in
                            case h of
                                (SOME e2, e3) =>
                                    if x = eval e2 p then
                                        e3
                                    else
                                        checkCases (x, tail) p
                            | (NONE, e3) =>
                                raise Impossible
                        end
                    | checkCases (x, _ ) p =
                        raise Impossible
            in
                eval (checkCases ((eval e1 p), matchList) p) p
            end
        | Prim1(oper, exp) => (* 14, 15, 16, 17, 18, 19 *)
            let in
                case eval exp p of
                    IntV x =>
                        let in
                            case oper of
                                "-" => IntV(~x)
                                | "print" =>
                                    let
                                        val printVariable = print(val2string((IntV x)) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | BoolV x =>
                        let in
                            case oper of
                                "!" => BoolV(not x)
                                | "print" =>
                                    let
                                        val printVariable = print(val2string((BoolV x)) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | SeqV x =>
                        let in
                            case oper of
                                "hd" =>
                                    let in
                                        if x <> [] then
                                            hd x
                                        else
                                            raise HDEmptySeq
                                    end
                                | "tl" =>
                                    let in
                                        if x <> [] then
                                            SeqV (tl x)
                                        else
                                            raise HDEmptySeq
                                    end
                                | "ise" =>
                                    let in
                                        case x of
                                            [] => BoolV true
                                            | _ => BoolV false
                                    end
                                | "print" =>
                                    let
                                        val printVariable = print(list2string(val2string, x) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | ListV x =>
                        let in
                            case oper of
                                "print" =>
                                    let
                                        val printVariable = print(list2string(val2string, x) ^ "\n")
                                    in
                                        ListV []
                                    end
                                | _ => raise Impossible
                        end
                    | _ => raise Impossible
            end
        | Prim2(";", e1, e2) => (*26*)
            let
                val aux = eval e1 p
            in
                eval e2 p
            end
        | Prim2(oper, e1, e2) => (*20, 21, 22, 23, 24, 25*)
            let
                val value1 = eval e1 p
                val value2 = eval e2 p
            in
                case (value1, value2) of
                    (IntV v1, IntV v2) =>
                        let in
                            case oper of
                                "+" => IntV(v1 + v2)
                                | "-" => IntV(v1 - v2)
                                | "*" => IntV(v1 * v2)
                                | "/" => IntV(v1 div v2)
                                | "<" => BoolV(v1 < v2)
                                | "<=" => BoolV(v1 <= v2)
                                | "=" => BoolV(v1 = v2)
                                | "!=" => BoolV(v1 <> v2)
                                | _ => raise Impossible
                            end
                    | (IntV v1, SeqV v2) =>
                        let in
                            case oper of
                                "::" => SeqV(IntV v1 :: v2)
                                | _ => raise Impossible
                        end
                    | (BoolV v1, BoolV v2) =>
                        let in
                            case oper of
                                "&&" => BoolV(v1 andalso v2)
                                | "=" => BoolV(v1 = v2)
                                | "!=" => BoolV(v1 <> v2)
                                | _ => raise Impossible
                        end
                    | (BoolV v1, SeqV v2) =>
                        let in
                            let in
                                case oper of
                                    "::" => SeqV(BoolV v1 :: v2)
                                    | _ => raise Impossible
                            end
                        end
                    | (ListV v1, SeqV v2) =>
                        let in
                            case oper of
                                "::" => SeqV(ListV v1 :: v2)
                                | _ => raise Impossible
                        end
                    | _ => raise Impossible
            end
;