open LD
open Expressions

let assertTrue condition description =
    if not condition then
        raise (System.Exception("Failed condition: " + description))
    else
        ()

let runTest name test =
    try
        printf "Running: %A "  name
        test()
        printfn "[SUCCEED]"

    with
        | e -> printfn "[FAILED: %A]" e.Message


let testReadChar () =
    let result = parse "hello" readChar
    assertTrue  (Option.isSome result) "Char read"
    assertTrue (match result with
                | Some ("h", _) -> true
                | _             -> false) "Identified result"

let testSimpleBlock () =
    let testPrj = "if x
   return a"
    let result = parse testPrj ifParser
    assertTrue  (Option.isSome result) "Block identified"
    assertTrue (match result with
                | Some (PIf (_, [PReturn _],[]) , _) -> true
                | _             -> false) "Identified result"

let testSimpleBlockWithCall () =
    let testPrj = "if x
   foo(1,2)
   return a"
    let result = parse testPrj ifParser
    assertTrue  (Option.isSome result) "Block with call identified"
    assertTrue (match result with
                | Some (PIf (_, [PCallStat(PCall("foo",[_; _])); PReturn _],[]) , _) -> true
                | _             -> false) "Identified result"
    

let testSimpleBlockWithEmptyLines () =
    let testPrj = "if x

   
   return a"
    let result = parse testPrj ifParser
    assertTrue  (Option.isSome result) "Block identified"
    assertTrue (match result with
                | Some (PIf (_, [PReturn _],[]) , _) -> true
                | _             -> false) "Identified result"


let testSimpleBlockWithEmptyLinesInTheMiddle () =
    let testPrj = "if x   
   return a
    
   return a"
    let result = parse testPrj ifParser
    assertTrue  (Option.isSome result) "Block identified"
    assertTrue (match result with
                | Some (PIf (_, [PReturn _; PReturn _],[]) , _) -> true
                | _             -> false) "Identified result"



let testTwoNestedBlocks () =
    let testPrj = "if x
   if y
     return z
   return a"
    let result = parse testPrj ifParser
    assertTrue  (Option.isSome result) "Block identified"
    assertTrue (match result with
                | Some (PIf (_,
                             [PIf(_, [PReturn (PSymbol "z")],[]);
                              PReturn (PSymbol "a")],[]) , _) -> true
                | _             -> false) "Identified result"


let testSimpleBinaryOperation () =
    let testPrj = "a + b"
    let result = parse testPrj pExpression
    assertTrue  (Option.isSome result) "Plus identified"
    assertTrue (match result with
                | Some (PBinaryOperation(Plus, (PSymbol "a"), (PSymbol "b")) , _) -> true
                | _             -> false) "Identified result"

let testSimpleBinaryOperationWithNestedOperand () =
    let testPrj = "a + (b+c)"
    let result = parse testPrj pExpression
    assertTrue  (Option.isSome result) "Plus identified"
    assertTrue (match result with
                | Some (PBinaryOperation(
                           Plus,
                           (PSymbol "a"),
                           (PNested(PBinaryOperation(Plus,(PSymbol "b"), (PSymbol "c"))) )
                           ), _ ) -> true
                | _             -> false) "Identified result"


let testArithmeticOperationParsing () =
    let testPrj = "x + y * b / 2"
    let result = parse testPrj pExpression
    assertTrue  (Option.isSome result) "Expression identified"
    assertTrue (match result with
                | Some (PBinaryOperation(
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
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PCall("foo",
                              [ PBinaryOperation(Plus, (PSymbol "a"), (PSymbol "b"));
                                PCall("goo",[])
                              ]), _) -> true
                | _             -> false) "Identified AST"


let testAndBooleanExpression () =
    let testCode = "a && b"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PBinaryOperation(And,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for And"
let testOrBooleanExpression () =
    let testCode = "a || b"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PBinaryOperation(Or,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for Or"
        
let testNotBooleanExpression () =
    let testCode = "!a"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PNot(PSymbol "a"), _) -> true
                | _    -> false) "Identified AST for Not"


let testSimpleEqualExpression () =
    let testCode = "a = b"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PBinaryOperation(Equal,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for Equal"

let testSimpleNotEqualExpression () =
    let testCode = "a <> b"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PBinaryOperation(NotEqual,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for not equal"

    
let testSimpleLessThanExpression () =
    let testCode = "a < b"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PBinaryOperation(Lt,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for less than"

let testSimpleGreaterThanExpression () =
    let testCode = "a > b"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PBinaryOperation(Gt,
                                         (PSymbol "a"),
                                         (PSymbol "b")), _) -> true
                | _    -> false) "Identified AST for greater than"

let testSimpleArrayAccess () =
    let testCode = "a[1]"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PArrayAccess((PSymbol "a"),
                                     (PNumber "1")), _) -> true
                | _    -> false) "Identified AST for simple array access"

let testStringParsing () =
    let testCode = "\"hello\""
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PString("hello"), _) -> true
                | _    -> false) "Identified AST for simple array access"



let testComposedAndExpressions1 () =
    let testCode = "x > 1 && x < 10"
    let result = parse testCode pExpression
    assertTrue  (Option.isSome result) "Parsed"
    assertTrue (match result with
                | Some (PBinaryOperation(
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
        ("parse simple string parsing", testStringParsing)
    
    
    
    ]

[<EntryPoint>]
let main args =
    printfn "Running tests tests %A" [||]
    for (testName, testFunc) in tests do
        runTest testName testFunc
    0


