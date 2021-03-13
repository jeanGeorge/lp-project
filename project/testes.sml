fun testCases (sourceCode:string, expected:expr) : string = 
  let
    val observed = fromString sourceCode    
  in
    if (observed = expected) then "true" else sourceCode
  end

val results = map (fn (s,e) => testCases(s, e)) cases;

fun isAllTestsPassed [] = true
  | isAllTestsPassed (h::t) = (h="true") andalso (isAllTestsPassed t);

val isTPCorrect = isAllTestsPassed(results);