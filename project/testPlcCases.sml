val cases =
  ((*Nil*)
    let val s = "()";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  ((*Boolean*)
    let val s = "true";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  ((*Integer*)
    let val s = "7";
        val e = "7 : Int"
    in
        (s, e)
    end
  ) ::
  ((*List*)
    let val s = "(1, 2)";
        val e = "(1, 2, ) : (Int, Int, )"
    in
        (s, e)
    end
  ) ::
  ((*List of three elements*)
    let val s = "((), true, 7)";
        val e = "((), true, 7, ) : (Nil, Bool, Int, )"
    in
        (s, e)
    end
  ) ::
  ((*Named Function with one argument*)
    let val s = "fun test (Bool x) = !x; test";
        val e = "<fun> : Bool -> Bool"
    in
        (s, e)
    end
  ) ::
  ((*Named Function with two arguments*)
    let val s = "fun test (Int x, Int y) = x + y; test";
        val e = "<fun> : (Int, Int, ) -> Int"
    in
        (s, e)
    end
  ) ::
  ((*Empty Sequence*)
    let val s = "([Bool] [])";
        val e = "[] : [Bool]"
    in
        (s, e)
    end
  ) ::
  ((*Sequence with four elements*)
    let val s = "(1::7::5::2::([Int] []))";
        val e = "[1, 7, 5, 2, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  ((*Boolean expression*)
    let val s = "3+1 = 4 && 4 <= 3";
        val e = "false : Bool"
    in
        (s, e)
    end
  ) ::
  ((*Head of a sequence*)
    let val s = "hd (1 :: 2 :: ([Int] []))";
        val e = "1 : Int"
    in
        (s, e)
    end
  ) ::
  ((*Match with SOME and NONE*)
    let val s = "var x = 2; match x with | 0 -> 1 | _ -> -1 end";
        val e = "~1 : Int"
    in
        (s, e)
    end
  )::
  ((*Is sequence empty for empty sequence*)
    let val s = "ise ([Int] [])";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  ((*Len Function*)
    let val s = "fun rec len([Int] l): Int = if ise(l) then 0 else 1 + len(tl(l)); len(1::2::([Int] []))";
        val e = "2 : Int"
    in
        (s, e)
    end
  ) ::
  ((*Comparisson between not equality types*)
    let val s = "var f = fn (Int x) => x end; (1, f, 3) = (1, f, 3)";
        val e = ExceptionMessageNotEqTypes
    in
        (s, e)
    end
  ) ::
  ((*Head of empty sequence*)
    let val s = "hd (([Int] []))";
        val e = ExceptionMessageEmptySeq
    in
        (s, e)
    end
  ) ::
  ((*Comparison between different types*)
    let val s = "true = 7";
        val e = ExceptionMessageNotEqTypes
    in
        (s, e)
    end
  ) ::
  ((*Wrong return type*)
    let val s = "fun rec f (Int x) : Int = true; f(5)";
        val e = ExceptionMessageWrongRetType
    in
        (s, e)
    end
  ) ::
  ((*If with different branch types*)
    let val s = "if 3 = 2 then 7 else false";
        val e = ExceptionMessageDiffBrTypes
    in
        (s, e)
    end
  ) ::
  ((*If condition not a boolean type*)
    let val s = "if 3 then 7 else 5";
        val e = ExceptionMessageIfCondNotBool
    in
        (s, e)
    end
  ) ::
  ((*Match without condition*)
    let val s = "var x = 2; match x with end";
        val e = ExceptionMessageNoMatchResults
    in
        (s, e)
    end
  ) ::
  ((*Match with different result types*)
    let val s = "var x = 2; match x with | 0 -> 1 | 1 -> true end";
        val e = ExceptionMessageMatchResTypeDiff
    in
        (s, e)
    end
  ) ::
  ((*Match with different codition types*)
    let val s = "var x = 2; match x with | 7 -> 1 | false -> -1 end";
        val e = ExceptionMessageMatchCondTypesDiff
    in
        (s, e)
    end
  )::
  ((*Call type mismatch exception*)
    let val s = "fun f (Int x) = x; f(true)";
        val e = ExceptionMessageCallTypeMisM
    in
        (s, e)
    end
  ) ::
  ((*Calling a non function type*)
    let val s = "var x = 7; x(7)";
        val e = ExceptionMessageNotFunc
    in
        (s, e)
    end
  ) ::
  ((*List index out of range*)
    let val s = "var x = (1, 2, 3); x[7]";
        val e = ExceptionMessageListOutOfRange
    in
        (s, e)
    end
  ) ::
  ((*Index access of a non list type*)
    let val s = "var x = 1; x[1]";
        val e = ExceptionMessageOpNonList
    in
        (s, e)
    end
  )::
  (
    let val s = "0";
        val e = "0 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "5+3*4";
        val e = "17 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "-3 < 4";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "!(3 = 4)";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "3+1 = 4 && 4 <= 3";
        val e = "false : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "if 3 = 2 then 0 else 1 + 4";
        val e = "5 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "3 + if 3 = 2 then 0 else 1";
        val e = "4 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "match x with | 0 -> 1 | _ -> -1 end";
        val e = ExceptionMessageSymbolNotFound
    in
        (s, e)
    end
  ) ::
  (
    let val s = "4; true";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "4 * (true; 6)";
        val e = "24 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "( )";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(1,false,())";
        val e = "(1, false, (), ) : (Int, Bool, Nil, )"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(1,(2,3),4)";
        val e = "(1, (2, 3, ), 4, ) : (Int, (Int, Int, ), Int, )"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(true,false)[1]";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "((5,6),false)[1][2]";
        val e = "6 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 + {3}";
        val e = "4 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "print false";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "print (1 - 3)";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Int] [])";
        val e = "[] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Int] [])";
        val e = "[] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Bool] [])";
        val e = "[] : [Bool]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Nil] [])";
        val e = "[] : [Nil]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([[Int]] [])";
        val e = "[] : [[Int]]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([Int -> Nil] [])";
        val e = "[] : [Int -> Nil]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([[Int -> Int -> Bool]] [])";
        val e = "[] : [[Int -> Int -> Bool]]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "([(Nil, Int, Bool)] [])";
        val e = "[] : [(Nil, Int, Bool, )]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 :: ([Int] [])";
        val e = "[1, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 :: 2 :: ([Int] [])";
        val e = "[1, 2, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "(1,2) :: (3,4) :: ([(Int,Int)] [])";
        val e = "[(1, 2, ), (3, 4, ), ] : [(Int, Int, )]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "hd (1 :: 2 :: ([Int] []))";
        val e = "1 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "tl (1 :: 2 :: ([Int] []))";
        val e = "[2, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "ise([Int] [])";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "ise(true::([Bool] []))";
        val e = "false : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; x+1";
        val e = "5 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "{var x = 4; x+1}";
        val e = "5 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; var y = 6; x + y";
        val e = "10 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var x = 4; print x; {var y = 6; print y }";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "1 + {var tmp = 9; x + x}";
        val e = ExceptionMessageSymbolNotFound
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var a = (3,4); a[1] < a[2]";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var e = ([Bool] []); true::false::e";
        val e = "[true, false, ] : [Bool]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fn (Int x) => x end";
        val e = "<fun> : Int -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var f = fn (Int x) => x end; f";
        val e = "<fun> : Int -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var f = fn (Int x) => x end; f";
        val e = "<fun> : Int -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var f = fn (Int x) => x end; f(10)";
        val e = "10 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f (Int x) = x; f";
        val e = "<fun> : Int -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f (Int x) = {fun g(Int y) = x+y; g}; f(3)(4)";
        val e = "7 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f (Int x) = fn (Int y) => x+y end; f(3)(4)";
        val e = "7 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = (
    "fun f (Int -> Bool g) = if g(1) then 10 else 11;"
    ^ "fun h (Int x) = 0 < x;"
    ^ "f(h)");
        val e = "10 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec f (Int x) : Int = if x <= 0 then 1 else x + f(x-1); f(5)";
        val e = "16 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec pr(Int x): Nil = if x <= 0 then print(0) else { print(x); pr(x-1) }; pr(5)";
        val e = "() : Nil"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec len([Int] l): Int = if ise(l) then 0 else 1 + len(tl(l)); len(1::2::([Int] []))";
        val e = "2 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fn (Int x, Int y) => x - y end";
        val e = "<fun> : (Int, Int, ) -> Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Int y) = x - y; f(5,4)";
        val e = "1 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var p = (1,3); fun f(Int x, Int y) = x - y; f(p)";
        val e = "~2 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Int y, Int z) = x - y * z ; f(5,4,2)";
        val e = "~3 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun rec mem(Int x, [Int] l): Bool = if ise(l) then false else if x = hd(l) then true else mem(x, tl(l)); mem(2, 1::2::([Int] []))";
        val e = "true : Bool"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun inc (Int x) = x + 1; fun add (Int x, Int y) = x + y; fun cadd (Int x) = fn (Int y) => x + y end; var y = add(3, inc(4)); var x = cadd(3)(7-y); var z = x * 3; fun rec fac (Int n) : Int = match n with | 0 -> 1 | 1 -> 1 | _ -> n * fac(n - 1) end; print x; print y; x :: y :: z :: fac(z) :: ([Int] [])";
        val e = "[2, 8, 6, 720, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "fun f(Int x, Bool b) = match b with | true -> {x + 1} | _    -> x end; f(3,true)";
        val e = "4 : Int"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var E = ([Int] []); fun reverse ([Int] l) = { fun rec rev ([Int] l1, [Int] l2): [Int] = if ise(l1) then l2 else rev(tl(l1), hd(l1)::l2); rev(l, E) }; reverse (1::2::3::E)";
        val e = "[3, 2, 1, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  (
    let val s = "var E = ([Int] []); fun reverse ([Int] s) = { fun rec rev ([Int] s1, [Int] s2): [Int] = match s1 with | E -> s2 | _ -> { var h = hd(s1); var t = tl(s1); rev(t, h::s2) } end; rev(s, E) }; reverse (1::2::3::E)";
        val e = "[3, 2, 1, ] : [Int]"
    in
        (s, e)
    end
  ) ::
  [ (
    let val s =
    "fun rec map ((Int -> Int) f) : ([Int] -> [Int]) = fn ([Int] l) => if ise(l) then l else f(hd(l)) :: map(f)(tl(l)) end; map (fn (Int x) => 2*x end) (10::20::30::([Int] []))";
        val e = "[20, 40, 60, ] : [Int]"
    in
        (s, e)
    end
  ) ];