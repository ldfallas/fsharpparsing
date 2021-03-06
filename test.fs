open Experiment
open Expressions

let assertTrue condition description =
    if not condition then
        raise (System.Exception("Failed condition: " + description))
    else
        ()
        
let assertFalse condition description =
    assertTrue (not condition) description

let isSuccess parsingResult =
    match parsingResult with
        | Success _ -> true
        | _         -> false
    

let runTest name test =
    try
        printf "Running: %A "  name
        test()
        printfn "[SUCCEED]"

    with
        | e -> printfn "[FAILED: %A]" e.Message


let testReadChar () =
    let result = parse "hello" readChar
    assertTrue  (isSuccess result) "Char read"
    assertTrue (match result with
                | Success ("h", _) -> true
                | _             -> false) "Identified result"

let testSimpleBlock () =
    let testPrj = "if x:
   return a"
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (_, [PReturn _], None) , _) -> true
                | _             -> false) "Identified result"

let testSimpleBlockWithAssignment () =
    let testPrj = "if x:
   a := x
   return a"
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (_, [PAssignStat(_,_); PReturn _],None) , _) -> true
                | _             -> false) "Identified result"

let testWhileWithCall () =
    let testPrj = "while x < 10:
    print(x)
    x := x + 1"
    let result = parse testPrj whileParser
    assertTrue  (isSuccess result) "While block with call identified"
    assertTrue (match result with
                | Success (PWhile (_, [PCallStat(PCall("print",[ _]));  _]) , _) -> true
                | _             -> false) "Identified result"

let testWhileWithPar () =
    let testPrj = "while ((x*x + y*y) < 2*2) && (iteration < maxiteration):
         xt := x * x - y*y + x0
         y := 2*x*y + y
         x := xt
         iteration := iteration + 1"

    let result = parse testPrj pStatement
    assertTrue  (isSuccess result) "While block with call identified"
    assertTrue (match result with
                | Success (PWhile (_, [_;_;_;_]), _ ) -> true
                | _             -> false) "Identified result"



let testSimpleBlockWithCall () =
    let testPrj = "if x:
   foo(1,2)
   return a"
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block with call identified"
    assertTrue (match result with
                | Success (PIf (_, [PCallStat(PCall("foo",[_; _])); PReturn _], None) , _) -> true
                | _             -> false) "Identified result"
    

let testSimpleBlockWithEmptyLines () =
    let testPrj = "if x:

   
   return a"
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (_, [PReturn _], None) , _) -> true
                | _             -> false) "Identified result"


let testSimpleBlockWithEmptyLinesInTheMiddle () =
    let testPrj = "if x:   
   return a
    
   return a"
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (_, [PReturn _; PReturn _], None) , _) -> true
                | _             -> false) "Identified result"



let testTwoNestedBlocks () =
    let testPrj = "if x:
   if y:
     return z
   return a"
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (_,
                             [PIf(_, [PReturn (PSymbol "z")], None);
                              PReturn (PSymbol "a")], None) , _) -> true
                | _             -> false) "Identified result"

let testTwoNestedBlocksWithSyntaxError () =
    let testPrj = "if x:
   if !@# y:
     return z
   return a"
    let result = parse testPrj ifParser
    assertFalse  (isSuccess result) "Block not  identified"
    assertTrue (match result with
                | Failure(Fatal(msg, 2)) -> true
                | _             -> false) "Identified result"


let testTwoNestedIfsWithElse () =
    let testPrj = "if x:
      if y:
        return z
      else:
        return k
      return a"
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (_,
                             [PIf(_, [PReturn (PSymbol "z")],
                                      Some([PReturn (PSymbol "k")]));
                              PReturn (PSymbol "a")], None) , _) -> true
                | _             -> false) "Identified result"

let testNestedIfsWithElseScenario () =
    let testPrj = "if x:
      if y:
        return z
      else:
        if z:
          return k
        else:
          if j:
            return y
          else:
             if m:
               return i
      return a
    "
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (PSymbol "x",
                             [PIf(PSymbol "y",
                                  [PReturn (PSymbol "z")],
                                   Some[PIf(PSymbol "z",
                                           [PReturn (PSymbol "k")],
                                            Some[PIf(PSymbol "j",
                                                    [PReturn (PSymbol "y")],
                                                     Some [PIf (PSymbol "m",
                                                               [PReturn (PSymbol "i")],None)])])]);
                              PReturn (PSymbol "a")],None), _) -> true                
                | _             -> false) "Identified result"


let testNestedIfsWithElseScenarioWithMixeEmptyLines () =
    let testPrj = "if x:
    
      if y:

    
        return z
    
      else:
    
        if z:
    
          return k
    
        else:
    
          if j:
    
            return y
    
          else:
    
             if m:
    
               return i
    
      return a
    "
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (PSymbol "x",
                             [PIf(PSymbol "y",
                                  [PReturn (PSymbol "z")],
                                   Some[PIf(PSymbol "z",
                                           [PReturn (PSymbol "k")],
                                            Some[PIf(PSymbol "j",
                                                    [PReturn (PSymbol "y")],
                                                     Some [PIf (PSymbol "m",
                                                               [PReturn (PSymbol "i")],None)])])]);
                              PReturn (PSymbol "a")],None), _) -> true                
                | _             -> false) "Identified result"






let testNestedBlocks () =
    let testPrj = "if x:
   x := 1
   if u = 1:
      zoo()
    
      if k = 1:
         foo()
         if z = 5:
            return 100
   if y:
     return z
   return a"
    let result = parse testPrj ifParser
    assertTrue  (isSuccess result) "Block identified"
    assertTrue (match result with
                | Success (PIf (_,
                             [PAssignStat(_, _);
                              PIf(_, _, _);
                              PIf(_, [PReturn (PSymbol "z")], None);
                              PReturn (PSymbol "a")], None) , _) -> true
                | _             -> false) "Identified result"



let testSimpleBinaryOperation () =
    let testPrj = "a + b"
    let result = parse testPrj pExpression
    assertTrue  (isSuccess result) "Plus identified"
    assertTrue (match result with
                | Success (PBinaryOperation(Plus, (PSymbol "a"), (PSymbol "b")) , _) -> true
                | _             -> false) "Identified result"

let testSimpleBinaryOperationWithNestedOperand () =
    let testPrj = "a + (b+c)"
    let result = parse testPrj pExpression
    assertTrue  (isSuccess result) "Plus identified"
    assertTrue (match result with
                | Success (PBinaryOperation(
                            Plus,
                            (PSymbol "a"),
                            (PNested(PBinaryOperation(Plus,(PSymbol "b"), (PSymbol "c"))) )
                            ), _ ) -> true
                | _             -> false) "Identified result"


let testArithmeticOperationParsing () =
    let testPrj = "x + y * b / 2"
    let result = parse testPrj pExpression
    assertTrue  (isSuccess result) "Expression identified"
    assertTrue (match result with
                | Success (PBinaryOperation(
                            Plus,
                            (PSymbol "x"),
                            (PBinaryOperation(
                                Div,
                                PBinaryOperation(
                                    Times,
                                    (PSymbol "y"),
                                    (PSymbol "b")),
                                (PNumber "2")))) , _) -> true
                | _             -> false) "Identified result"
    
    
let testCallWithNestedExpressions () =
    let testPrj = "foo(a + b, goo())"
    let result = parse testPrj pMultiplicativeExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PCall("foo",
                              [ PBinaryOperation(Plus, (PSymbol "a"), (PSymbol "b"));
                                PCall("goo",[])
                              ]), _) -> true
                | _             -> false) "Identified AST"


let testAndBooleanExpression () =
    let testCode = "a && b"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PBinaryOperation(And,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for And"
let testOrBooleanExpression () =
    let testCode = "a || b"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PBinaryOperation(Or,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for Or"
        
let testNotBooleanExpression () =
    let testCode = "!a"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PNot(PSymbol "a"), _) -> true
                | _    -> false) "Identified AST for Not"


let testSimpleEqualExpression () =
    let testCode = "a = b"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PBinaryOperation(Equal,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for Equal"

let testSimpleNotEqualExpression () =
    let testCode = "a <> b"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PBinaryOperation(NotEqual,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for not equal"

    
let testSimpleLessThanExpression () =
    let testCode = "a < b"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PBinaryOperation(Lt,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for less than"

let testSimpleGreaterThanExpression () =
    let testCode = "a > b"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PBinaryOperation(Gt,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for greater than"

let testSimpleArrayAccess () =
    let testCode = "a[1]"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PArrayAccess((PSymbol "a"),
                                     (PNumber "1")), _) -> true
                | _    -> false) "Identified AST for simple array access"

let testStringParsing () =
    let testCode = "\"hello\""
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PString("hello"), _) -> true
                | _    -> false) "Identified AST for simple array access"



let testComposedAndExpressions1 () =
    let testCode = "x > 1 && x < 10"
    let result = parse testCode pExpression
    assertTrue  (isSuccess result) "Parsed"
    assertTrue (match result with
                | Success (PBinaryOperation(
                             And, 
                             PBinaryOperation(
                               Gt,
                               (PSymbol "x"),
                               (PNumber "1")),
                             PBinaryOperation(
                               Lt,
                               (PSymbol "x"),
                               (PNumber "10")
                         )), _) -> true
                | _    -> false) "Identified AST for && of comparison expressions"
     



let tests  = [
    ("readChar", testReadChar);
    ("simpleIndentationBlock", testSimpleBlock);
    ("simpleIndentationBlockWithCall", testSimpleBlockWithCall);
    ("testSimpleBlockWithEmptyLines", testSimpleBlockWithEmptyLines);
    ("testSimpleBlockWithEmptyLinesInThemiddle", testSimpleBlockWithEmptyLinesInTheMiddle);
    ("two nested blocks", testTwoNestedBlocks) ;
    ("several nested blocks", testNestedBlocks) ;
    ("parse a + b", testSimpleBinaryOperation) ;
    ("parse a + (b+c)", testSimpleBinaryOperationWithNestedOperand);
    ("parse: foo(a + b, goo())", testCallWithNestedExpressions);
    ("parse arithmetic operation", testArithmeticOperationParsing);
    ("parse basic and expression", testAndBooleanExpression);
    ("parse basic or expression", testOrBooleanExpression);
    ("parse basic not expression", testNotBooleanExpression);
    ("parse basic equal expression", testSimpleEqualExpression);
    ("parse basic not equal expression", testSimpleNotEqualExpression);
    ("parse basic less than expression", testSimpleLessThanExpression);
    ("parse basic greater than expression", testSimpleGreaterThanExpression);
    ("parse simple array access", testSimpleArrayAccess);
    ("parse simple and of comparison operations", testComposedAndExpressions1);
    ("Parse assignment  statement", testSimpleBlockWithAssignment);
    ("parse simple string parsing", testStringParsing) ;
    ("Parse nested ifs with else", testTwoNestedIfsWithElse) ;
    ("test nested ifs nightmare scenario", testNestedIfsWithElseScenario);
    ("test nested ifs nightmare scenario with empty lines", testNestedIfsWithElseScenarioWithMixeEmptyLines) ;
    ("test while with call", testWhileWithCall);
    ("test while with parenthesis", testWhileWithPar);
    ("test if with syntax error", testTwoNestedBlocksWithSyntaxError)
    
    ]

[<EntryPoint>]
let main args =
    printfn "Running tests tests %A" [||]
    for (testName, testFunc) in tests do
        runTest testName testFunc
    0


