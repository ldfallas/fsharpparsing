namespace Experiment


open Expressions


type PFunction =
    | BuiltInFunction of ((PExpr list) -> PExpr)



type  EvaluatorState = {
    parent: Option<EvaluatorState> ;
    definitions: System.Collections.Generic.Dictionary<string, PExpr>
    functions: System.Collections.Generic.Dictionary<string, PFunction>
}




module Evaluator =
    let lookup_state (name:string) (state:EvaluatorState) : Option<PExpr> =
        match (state.definitions.TryGetValue(name)) with
        | (true, expr) -> Some expr
        | (false, _) -> None
    let assign_in_state (name:string) (value:PExpr) (state:EvaluatorState) =
        state.definitions.[name] <- value
    let lookup_function (name:string) (state:EvaluatorState) : Option<PFunction> =
        match (state.functions.TryGetValue(name)) with
        | (true, expr) -> Some expr
        | (false, _) -> None
(*
   type Operator =
    | Plus
    | Minus
    | Times
    | Div
    | Mod
    | And
    | Or
    | Equal
    | NotEqual
    | Assign
    | Lt
    | Gt
    

type PExpr =
    | PSymbol  of string
    | PString  of string
    | PNumber  of string
    | PBoolean of bool
    | PNot     of PExpr
    | PCall    of string * (PExpr list)
    | PNested  of PExpr
    | PArrayAccess of PExpr * PExpr
    | PBinaryOperation of Operator * PExpr * PExpr

type PStat =
    | PIf of PExpr * (PStat list) * ((PStat list) option)
    | PWhile of PExpr * (PStat list)
    | PCallStat of PExpr
    | PAssignStat of PExpr * PExpr
    | PReturn of PExpr

   *)

    let println(str: string) =
        System.Console.WriteLine(str)

    let print(str: string) =
        System.Console.Write(str)
    let spaces(number:int) =
        for i = 0 to number do
            System.Console.Write(' ')
    let rec print_expression(ast: PExpr) =
        match ast with
        | PSymbol(symbol) -> print(symbol)
        | PString(str)    -> print("\"" + str + "\"")
        | PNumber(num)    -> print(num)
        | PBoolean(boolValue) -> print(boolValue.ToString())
        | PNot(innerExpr)     -> print("not")
                                 print_expression(innerExpr)
        | PCall(funcName,args) ->
            print(funcName)
            print("(")
            for arg in args do
                print_expression(arg)
                print(" ")
            print(")")
            
        | PNested(innerExpr) ->
               print("(")
               print_expression(innerExpr)
               print(")")
        | PArrayAccess(arrayExpr, indexExpr) ->
               print_expression(arrayExpr)
               print("[")
               print_expression(indexExpr)
               print("]")            
        | PBinaryOperation(op, left, right) ->
            print_expression(left)
            match op with
            | Plus  -> print(" + ")
            | Minus  -> print(" - ")
            | Times  -> print(" * ")
            | Div  -> print(" / ")
            | Mod  -> print(" % ")
            | And  -> print(" && ")
            | Or  -> print(" || ")
            | Equal  -> print(" = ")
            | NotEqual  -> print(" <> ")
            | Assign  -> print(" := ")
            | Lt  -> print(" < ")
            | Gt  -> print(" > ")

            print_expression(left)

            

    let rec print_tree (ast: PStat) indentationLevel =
        match ast with
        | PIf(condition, thenStat, Some(elseStat)) ->
            spaces(indentationLevel)
            System.Console.Write("if ")
            print_expression(condition)
            println(":")
            print_stats(thenStat, indentationLevel + 1)
            println("")
            print_stats(elseStat, indentationLevel + 1)
            println("")
            
        | PIf(condition, thenStat, None) ->
            spaces(indentationLevel)
            System.Console.Write("if ")
            print_expression(condition)
            println(":")
            print_stats(thenStat, indentationLevel + 1)            
        | PWhile(condition,body) ->
            spaces(indentationLevel)            
            print("while ")
            print_expression(condition)
            println("")
            print_stats(body, indentationLevel + 1)
        | PCallStat(expr) ->
            spaces(indentationLevel)
            print_expression(expr)
        | PAssignStat(left, right) ->
            spaces(indentationLevel)
            print_expression(left)
            print(" = ")
            print_expression(right)
            println("")
        | PReturn(ret) ->
            spaces(indentationLevel)
            print("return ")
            print_expression(ret)
            println("")                        
    and  print_stats(stats: PStat list, indentationLevel) =
        for stat in stats do
            spaces(indentationLevel)
            print_tree stat indentationLevel

    let evaluate_binary_operation op exp1 exp2 =
            match op with
            | Plus ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) -> (System.Double.Parse(n1) + System.Double.Parse(n2)).ToString() |> PNumber
                | _ -> raise (System.Exception("Incompatible arguments for +"))
            | Minus ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) -> (System.Double.Parse(n1) - System.Double.Parse(n2)).ToString() |> PNumber
                | _ -> raise (System.Exception("Incompatible arguments for -"))
                
            | Times ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) -> (System.Double.Parse(n1) * System.Double.Parse(n2)).ToString() |> PNumber
                | _ -> raise (System.Exception("Incompatible arguments for *"))
            | Div ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) -> (System.Double.Parse(n1) / System.Double.Parse(n2)).ToString() |> PNumber
                | _ -> raise (System.Exception("Incompatible arguments for /"))
            | Mod ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) -> (System.Double.Parse(n1) % System.Double.Parse(n2)).ToString() |> PNumber
                | _ -> raise (System.Exception("Incompatible arguments for %"))
                
            | And ->
                match(exp1, exp2) with
                | (PBoolean(n1), PBoolean(n2)) -> PBoolean(n1 && n2)
                | _ -> raise (System.Exception("Incompatible arguments for and"))                        
            | Or ->
                match(exp1, exp2) with
                | (PBoolean(n1), PBoolean(n2)) -> PBoolean(n1 || n2)
                | _ -> raise (System.Exception("Incompatible arguments for or"))
            | Equal ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) -> PBoolean(n1 = n2)
                | _ -> PBoolean(false)
            | NotEqual ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) -> PBoolean(n1 <> n2)
                | _ -> PBoolean(false)
            
            | Assign ->
                raise (System.Exception("Incompatible arguments for or"))
            | Lt ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) ->
                    (System.Double.Parse(n1) < System.Double.Parse(n2)) |> PBoolean
                | _ -> raise (System.Exception("Incompatible arguments for *"))            
            | Gt ->
                match(exp1, exp2) with
                | (PNumber(n1), PNumber(n2)) -> (System.Double.Parse(n1) > System.Double.Parse(n2)) |> PBoolean
                | _ -> raise (System.Exception("Incompatible arguments for *"))            
                

    let evaluate_function (func:PFunction) (evaluatedArguments: PExpr list) (state: EvaluatorState) =
        match func with
        | BuiltInFunction f -> f evaluatedArguments
                
    let rec evaluate_expression expr state =
        match expr with
        | PSymbol(symbolName) ->
            match (lookup_state symbolName state) with
            | Some(value) -> value
            | _ -> raise (System.Exception("Cannot find symbol: " + symbolName)) 
        | PString(_) -> expr
        | PNumber(_) -> expr
        | PBoolean(_) -> expr
        | PNot(negated) ->
            match(evaluate_expression negated state) with
            | PBoolean(false) -> PBoolean(true)
            | _ -> PBoolean(false)
//        | PCall    of string * (PExpr list)
        | PNested(innerExpr) -> evaluate_expression innerExpr state
        | PArrayAccess(_,_) ->
             raise (System.Exception("Array access not implemented"))

        | PBinaryOperation(op, exp1, exp2) ->
            let exp1 = evaluate_expression exp1 state
            let exp2 = evaluate_expression exp2 state
            evaluate_binary_operation op exp1 exp2
        | PCall(name, arguments) ->
            let evaluatedArguments = (List.map (fun arg -> evaluate_expression arg state) arguments)
            match( lookup_function name state) with
            | Some(afunction) -> evaluate_function afunction evaluatedArguments state
            | _ -> raise (System.Exception("Cannot find function: " + name)) 


    let initialState:EvaluatorState =
        let funcs = System.Collections.Generic.Dictionary<string, PFunction>()
        funcs.["print"] <- BuiltInFunction (fun (e) ->
            match e with
            | [ PNumber(n) ] -> System.Console.Write( n )
                                PNumber("0")
            | [ PString(s) ] -> System.Console.Write( s )
                                PNumber("0")
            | _ -> raise (System.Exception("Not implemented print "+ e.ToString())))
        funcs.["println"] <- BuiltInFunction (fun (e) ->
            match e with
            | [ PNumber(n) ] -> System.Console.WriteLine( n )
                                PNumber("0")
            | [ PString(s) ] -> System.Console.WriteLine( s )
                                PNumber("0")
            | _ -> raise (System.Exception("Not implemented print "+ e.ToString())))
            

        { parent = None;
          definitions = System.Collections.Generic.Dictionary<string, PExpr>();
          functions = funcs }

    let rec evaluate (stat:PStat) (state:EvaluatorState) =
        match stat with
            | PCallStat(expr) -> evaluate_expression expr state  |> ignore
            | PIf(condition, thenStats , elseStatsOp) ->
                 let evaluatedCondition = evaluate_expression condition state
                 match evaluatedCondition, elseStatsOp with
                 | (PBoolean(true), _) ->
                     for thenStat in thenStats do
                         evaluate thenStat state
                 | (_, Some(elseStats)) ->
                     for elseStat in elseStats do
                           evaluate elseStat state
                 | _,_ -> ignore null
            | PWhile(condition, blockStats) ->
                 while (match (evaluate_expression condition state) with
                        | PBoolean(true) -> true
                        | _ -> false) do
                     for whileStat in blockStats do
                           evaluate whileStat state
                 
            | PAssignStat(left, right) ->
                let evaluated_value = evaluate_expression right state
                match left with
                | PSymbol variable ->
                    (assign_in_state variable evaluated_value state) |> ignore

                | _ -> System.Console.WriteLine("Cannot perform assignment")
            | _ -> System.Console.WriteLine("Unknown statement")

    let main =
        let sb = System.Text.StringBuilder()
        let mutable line =  ""
        line <- System.Console.ReadLine()
        while(  line <> null) do
            sb.Append(line + "\n") |> ignore
            line <- System.Console.ReadLine()
        let code = sb.ToString()
        System.Console.WriteLine("Code to process:" + code)
        let pResult = parse code pStatement
        match pResult with
            | Success(stat, _) ->
                print_tree stat 0
                (evaluate stat initialState)
            | Failure(Fatal(msg,line)) -> System.Console.WriteLine("Fatal parsing error: " + msg + ":"+ line.ToString())
            | Failure(Fail) -> System.Console.WriteLine("Unexpected error")
            
