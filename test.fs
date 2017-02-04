open LD
open Expressions

let assertTrue condition description =
    if not condition then
        raise (System.Exception("Failed condition: " + description))
    else
        ()

let runTest name test =
    try
        printfn "Running: %A "  name
        test()
        printfn "[SUCCEED]\n"

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
    let result = parse testPrj pArithExpression
    assertTrue  (Option.isSome result) "Plus identified"
    assertTrue (match result with
                | Some (PBinaryOperation(Plus, (PSymbol "a"), (PSymbol "b")) , _) -> true
                | _             -> false) "Identified result"

let testSimpleBinaryOperationWithNestedOperand () =
    let testPrj = "a + (b+c)"
    let result = parse testPrj pArithExpression
    assertTrue  (Option.isSome result) "Plus identified"
    assertTrue (match result with
                | Some (PBinaryOperation(
                           Plus,
                           (PSymbol "a"),
                           (PBinaryOperation(Plus,(PSymbol "b"), (PSymbol "c"))) ), _ ) -> true
                | _             -> false) "Identified result"
    


let tests  = [
    ("readChar", testReadChar);
    ("simpleIndentationBlock", testSimpleBlock);
    ("two nested blocks", testTwoNestedBlocks) ;
    ("parse a + b", testSimpleBinaryOperation) ;
    ("parse a + (b+c)", testSimpleBinaryOperationWithNestedOperand)
    
    ]

[<EntryPoint>]
let main args =
    printfn "Running tests tests %A" [||]
    for (testName, testFunc) in tests do
        runTest testName testFunc
    0


 
