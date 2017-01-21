namespace LD


type SAtom =
    | SSymbol of string
    | SString of string
    | SNumber of string


type SList =
    | SCons of SAtom * SList
    | Nil
    
type Operator =
    | Plus
    | Minus
    | Times
    | Div

type PExpr =
    | PSymbol of string
    | PString of string
    | PNumber of string
    | PCall of string * (PExpr list)
    | BinaryOperation of Operator * PExpr * PExpr

type PStat =
    | PIf of PExpr * (PStat list) * (PStat list)
    | PReturn of PExpr


module Expressions =
        

   type ReaderState = { Data : string;
                        Position: int;
                        Indentation: int list}

   let concatParser (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                    (parser2 : (ReaderState ->  (('b * ReaderState) option ))) =
       fun input -> match (parser1 input) with
                    | Some (_, restState) -> parser2 restState
                    | _ -> None

   let concatParser2 (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                     (parser2N : ('a ->  (ReaderState ->  (('b * ReaderState) option )))) =
       fun input -> match (parser1 input) with
                    | Some (matchedTxt, restState) -> (parser2N matchedTxt) restState
                    | _ -> None

   let inline (>>) (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                    (parser2 : (ReaderState ->  (('b * ReaderState) option ))) = concatParser parser1 parser2

   let inline (>>=) (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                    (parser2N : ('a ->  (ReaderState ->  (('b * ReaderState) option )))) = concatParser2 parser1 parser2N
                    
   let disjParser (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                  (parser2 : (ReaderState ->  (('a * ReaderState) option ))) =
       fun input -> match (parser1 input) with
                    | result & Some _ -> result
                    | _ -> parser2 input

   let preturn aValue (state : ReaderState ) = Some (aValue, state)

   let pGetIndentation (state : ReaderState ) =
       Some (state.Indentation, state)
   let pSetIndentation newIndentationLevel (state : ReaderState ) =
       Some (newIndentationLevel,
             { state with Indentation = newIndentationLevel :: state.Indentation } )
   let pSetFullIndentation newIndentation (state : ReaderState ) =
       Some (newIndentation,
             { state with Indentation = newIndentation } )


   let pfail (state : ReaderState) = None


   let rec readingChar currentIndex (predicate:char -> bool) (data : string) : int =
       if data.Length > currentIndex && predicate data.[currentIndex] then
           readingChar (currentIndex + 1) predicate data
       else
           currentIndex
           
   let readZeroOrMoreChars (pred:char -> bool) (state : ReaderState) : (string * ReaderState) option =
       let
         secondPosition = readingChar state.Position pred state.Data
         in
           Some ( state.Data.Substring(state.Position,
                                  secondPosition - state.Position),
                 { state with Position = secondPosition })

           

   let whitespace = readZeroOrMoreChars System.Char.IsWhiteSpace

   let whitespaceNoNl =
       readZeroOrMoreChars
          (fun c -> System.Char.IsWhiteSpace(c) && c <> '\n')
                                  

   let readChar(state : ReaderState) : (string * ReaderState) option =
       if state.Data.Length > state.Position then
          Some (state.Data.[state.Position].ToString(),
                { state with Position = state.Position + 1 } )
       else
          None

   let readWithCondition pred state : (string * ReaderState) option =
         Option.filter pred (readChar state)

   let readSpecificChar myChar (state : ReaderState) : (string * ReaderState) option =
       readWithCondition (fun (r, state) -> r.[0] = myChar) state

   let readWithConditionOnChar pred state : (string * ReaderState) option =
         Option.filter (fun (c, _) -> pred c) (readChar state)
       

   let readLPar =
       concatParser whitespace (readSpecificChar '(')
   let readRPar = readSpecificChar ')'

   let symbol =
       concatParser2
          (readWithConditionOnChar  (fun c -> System.Char.IsLetter(c, 0)))
          (fun initialChar ->
               concatParser2
                  (readZeroOrMoreChars (fun c -> System.Char.IsLetter(c) || System.Char.IsDigit(c)))
                  (fun suffixString -> (preturn (PSymbol (initialChar + suffixString))))
           )


   let optional (parser : (ReaderState ->  (('a * ReaderState) option ))) (defaultValue:'a) =
       fun input -> match (parser input) with
                    | result & Some _ -> result
                    | _ -> (Some (defaultValue, input))

   let digitP = readWithConditionOnChar  (fun c -> System.Char.IsDigit(c, 0))
   let digitsP = (readZeroOrMoreChars (fun c ->  System.Char.IsDigit(c)))
   let decimalPartP = (readSpecificChar '.' >>= (fun dot ->
                       digitP  >>= (fun firstChar -> 
                       digitsP >>= (fun digits  ->
                        preturn (dot + firstChar + digits)))))
                           
    
   let number =
              ( (optional (readSpecificChar '-') "") >>= (fun neg -> 
                digitP  >>= (fun firstChar -> 
                (readZeroOrMoreChars (fun c ->  System.Char.IsDigit(c))) >>= (fun chars ->
                (optional decimalPartP "") >>= (fun dec ->                                                                               
                preturn (PNumber (neg + firstChar + chars + dec)))))))
       

   let pkeyword name =
       concatParser2
          symbol
          (fun result ->
              match result with
              | PSymbol symbolName
                  when symbolName = name -> preturn result
              | _ -> pfail)

   let ifKeyword = pkeyword "if"
   let returnKeyword = pkeyword "return"
   
   let colon  =
       concatParser whitespaceNoNl (readSpecificChar ':')
       
   let newline  =
       concatParser whitespaceNoNl (readSpecificChar '\n')

   let identifyOperator operatorChar operatorResult =
       concatParser
          whitespaceNoNl
          ((readSpecificChar operatorChar) >> (preturn operatorResult))
          
   let plusOperator = identifyOperator '+' Plus
   let minusOperator = identifyOperator '-' Minus
   let pTimesOperator = identifyOperator '*' Times
   let pDivOperator = identifyOperator '*' Div
       

   let indentation =
       (concatParser2
          pGetIndentation
          (fun indentation ->
             (concatParser2
                (readZeroOrMoreChars (fun c -> c = ' '))              
                (fun spaces ->
                   match (spaces.Length,
                          indentation) with
                   | (lessThan, top::_) when lessThan > top ->
                       (pSetIndentation lessThan) >> (preturn "INDENT")
                   | (lessThan, top::_) when lessThan = top ->
                       preturn "INDENTED"
                   | (identifiedIndentation, top::rest) ->
                          (pSetFullIndentation rest) >> (preturn "DEDENT")
                   | _ -> pfail))))

   let indent = indentation >>= (fun result -> if result = "INDENT" then preturn result else pfail)
   let dedent = indentation >>= (fun result -> if result = "DEDENT" then preturn result else pfail)
   let indented = indentation >>= (fun result -> if result = "INDENTED" then preturn result else pfail)



   let rec oneOrMore parser accumulated =
       parser >>=
           (fun lastResult ->
              let result = lastResult::accumulated
              in disjParser (oneOrMore parser result) (preturn (List.rev result)))
           
   let zeroOrMore parser accumulated =
       disjParser (oneOrMore parser accumulated) (preturn accumulated)


   let pExpression = whitespace >> (disjParser symbol number)


   let buildExpressions (leftExpr:PExpr) (rightExprs:(Operator * PExpr) list) =
       (List.fold (fun left (op,right) -> BinaryOperation(op, left, right)) leftExpr rightExprs)

   let pBinaryExpression operators lowerLevelElementParser  =
       lowerLevelElementParser
         >>= (fun leftTerm ->
                 (zeroOrMore
                     (operators
                        >>= (fun op ->
                               lowerLevelElementParser >>= (fun expr -> preturn (op, expr))))
                     [])
                   >>= (fun acc -> preturn (buildExpressions leftTerm acc) ))

   let pTerm = pBinaryExpression (disjParser pTimesOperator pDivOperator)  pExpression
         
   let pArithExpression = pBinaryExpression (disjParser plusOperator minusOperator)  pTerm

   let pReturn  = returnKeyword >>
                  pExpression >>=
                      (fun expr -> preturn (PReturn expr))

   let pStatements = ref []

   let pStatement =
       fun state -> (List.reduce disjParser !pStatements) state

   let commaP = whitespace >> (readSpecificChar ',') 

   let simpleSeq = pExpression >>= (fun first ->
                   (zeroOrMore (commaP  >> pExpression) []) >>= (fun exprs ->
                   preturn (first::exprs)))


// (pStatement >>= (fun stat -> newline >> (preturn stat)))
   let pBlock = 
       indent >> (pStatement >>=
          (fun firstStat ->
                ((zeroOrMore (newline >> indented >> pStatement) [])
                 >>= (fun restStats -> dedent >> preturn (firstStat::restStats)))))


   let ifParser =
       ifKeyword  >>
       pExpression >>=
          (fun expr -> newline >> pBlock >>= (fun block -> preturn (PIf(expr, block, []))))
             


   let testParser =
       concatParser 
          whitespace 
          (concatParser 
              (readSpecificChar '(')
              (concatParser 
                  whitespace
                  (concatParser
                     (readSpecificChar ')') 
                     (preturn Nil)
                     )

                   ) 
           )

   let testParser2 =
       concatParser 
          whitespace 
          (concatParser 
              (readSpecificChar '(')
              (concatParser 
                  whitespace
                  (concatParser
                     (readSpecificChar ')') 
                     (preturn Nil)
                     )

                   ) 
           )

   pStatements := [pReturn; ifParser]

   let parse (input : string) (parser : (ReaderState ->  (('a * ReaderState) option ))) =
       parser {  Data = input ; Position = 0; Indentation = [0] }
