
namespace Experiment

type Operator =
    | Plus
    | Minus
    | Times
    | Div
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
    | PCallStat of PExpr
    | PAssignStat of PExpr * PExpr
    | PReturn of PExpr


module Expressions =
        

   type ReaderState = { Data : string;
                        Position: int;
                        Indentation: int list}

   let concatParsers (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                     (parser2 : (ReaderState ->  (('b * ReaderState) option ))) =
       fun input -> match (parser1 input) with
                    | Some (_, restState) -> parser2 restState
                    | _ -> None

   let concatParsers2 (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                     (parser2N : ('a ->  (ReaderState ->  (('b * ReaderState) option )))) =
       fun input -> match (parser1 input) with
                    | Some (matchedTxt, restState) -> (parser2N matchedTxt) restState
                    | _ -> None

   let inline (>>) (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                    (parser2 : (ReaderState ->  (('b * ReaderState) option ))) = concatParsers parser1 parser2

   let inline (>>=) (parser1 : (ReaderState ->  (('a * ReaderState) option )))
                    (parser2N : ('a ->  (ReaderState ->  (('b * ReaderState) option )))) = concatParsers2 parser1 parser2N
                    
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


   let pfail (state : ReaderState) =
       //System.Console.Out.WriteLine("Failing at: " + state.Position.ToString()-)
       None


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



   let rec readingStringChar currentIndex (predicate:char -> char -> bool) (data : string) (previousChar : char) : int =
       if data.Length > currentIndex && (predicate previousChar data.[currentIndex]) then
           readingStringChar (currentIndex + 1) predicate data data.[currentIndex]
       else
           currentIndex
           
   let readZeroOrMoreStringChars (pred:char -> char -> bool) (state : ReaderState) : (string * ReaderState) option =
       let
         secondPosition = readingStringChar state.Position pred state.Data '\000'
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
       concatParsers whitespace (readSpecificChar '(')
   let readRPar = readSpecificChar ')'



   let pSymbol =
       concatParsers2
          (readWithConditionOnChar  (fun c -> System.Char.IsLetter(c, 0)))
          (fun initialChar ->
               concatParsers2
                  (readZeroOrMoreChars (fun c -> System.Char.IsLetter(c) || System.Char.IsDigit(c)))
                  (fun suffixString -> (preturn (PSymbol (initialChar + suffixString))))
           )


   let optionalP (parser : (ReaderState ->  (('a * ReaderState) option ))) (defaultValue:'a) =
       fun input -> match (parser input) with
                    | result & Some _ -> result
                    | _ -> (Some (defaultValue, input))

   let digitP = readWithConditionOnChar  (fun c -> System.Char.IsDigit(c, 0))
   let digitsP = (readZeroOrMoreChars (fun c ->  System.Char.IsDigit(c)))
   let decimalPartP = (readSpecificChar '.' >>= (fun dot ->
                       digitP  >>= (fun firstChar -> 
                       digitsP >>= (fun digits  ->
                        preturn (dot + firstChar + digits)))))
                           
    
   let pNumber =
              ( (optionalP (readSpecificChar '-') "") >>= (fun neg -> 
                digitP  >>= (fun firstChar -> 
                (readZeroOrMoreChars (fun c ->  System.Char.IsDigit(c))) >>= (fun chars ->
                (optionalP decimalPartP "") >>= (fun dec ->                                                                               
                preturn (PNumber (neg + firstChar + chars + dec)))))))


   let pString =
       whitespace >>
       readSpecificChar '"' >>
       readZeroOrMoreStringChars (fun previous current ->
                 (previous = '\\' && current = '"') || current <> '"')
       >>= (fun stringContents ->
            readSpecificChar '"' >> (preturn (PString stringContents)))
       

   let pkeyword name =
       concatParsers2
          pSymbol
          (fun result ->
              match result with
              | PSymbol symbolName
                  when symbolName = name -> preturn result
              | _ -> pfail)

   let ifKeyword     = pkeyword "if"
   let returnKeyword = pkeyword "return"
   let elseKeyword   = pkeyword "else"

   let simpleCharWithoutWhitespace theCharacter =
       concatParsers whitespaceNoNl (readSpecificChar theCharacter)
   
   let colon  =
       simpleCharWithoutWhitespace ':'
       
   let newline  =
       concatParsers whitespaceNoNl (readSpecificChar '\n')

   let pLSquareBracket  =
       simpleCharWithoutWhitespace '['

   let pRSquareBracket  =
       simpleCharWithoutWhitespace ']'


   let identifyOperator operatorChar operatorResult =
       concatParsers
          whitespaceNoNl
          ((readSpecificChar operatorChar) >> (preturn operatorResult))

   let identifyOperator2 operatorChar1 operatorChar2 operatorResult =
       concatParsers
          whitespaceNoNl
          ((readSpecificChar operatorChar1) >>
           (readSpecificChar operatorChar2) >>
           (preturn operatorResult))
          
   let pAssignOperator =
       whitespaceNoNl >>
       (readSpecificChar ':') >>
       (readSpecificChar '=') 
         
   let plusOperator = identifyOperator '+' Plus
   let minusOperator = identifyOperator '-' Minus
   let pTimesOperator = identifyOperator '*' Times
   let pDivOperator = identifyOperator  '/' Div
   let pGtOperator = identifyOperator  '>' Gt
   let pLtOperator = identifyOperator  '<' Lt
   let pEqualOperator = identifyOperator  '=' Equal      
   let pAndOperator = identifyOperator2 '&' '&' And
   let pNotEqualOperator = identifyOperator2 '<' '>' NotEqual   
   let pOrOperator = identifyOperator2  '|' '|' Or
   let pNotOperator = concatParsers whitespaceNoNl (readSpecificChar '!')   
       



   let rec oneOrMore parser accumulated =
       parser >>=
           (fun lastResult ->
              let result = lastResult::accumulated
              in disjParser (oneOrMore parser result) (preturn (List.rev result)))
           
   let zeroOrMore parser accumulated =
       disjParser (oneOrMore parser accumulated) (preturn accumulated)


   let pTopExpressions = ref []

   let pExpression =
       fun state -> (List.reduce disjParser !pTopExpressions) state
       

   let pNested = readLPar    >>
                 pExpression >>= (fun expr ->
                 readRPar    >>  (preturn (PNested expr)))

   let commaP = whitespace >> (readSpecificChar ',')

   let simpleSeq  =
       disjParser (pExpression >>= (fun first ->
                   (zeroOrMore (commaP  >> pExpression) []) >>= (fun exprs ->
                   preturn (first::exprs)))) (preturn [])
                  

   let pCall =
       whitespace >>
       pSymbol >>= (fun funcNameRef ->
                   match funcNameRef with
                   | (PSymbol funcName) ->
                        readLPar >>
                        simpleSeq >>= (fun args ->
                        readRPar >> (preturn (PCall(funcName, args))))
                   | _ -> pfail)
           

   let pPrimaryExpression = whitespace >> (List.reduce disjParser [ pSymbol; pNumber; pString; pNested])


   let buildExpressions (leftExpr:PExpr) (rightExprs:(Operator * PExpr) list) =
       (List.fold (fun left (op,right) -> PBinaryOperation(op, left, right)) leftExpr rightExprs)

   let pBinaryExpression operators lowerLevelElementParser  =
       lowerLevelElementParser
         >>= (fun leftTerm ->
                 (zeroOrMore
                     (operators
                        >>= (fun op ->
                               lowerLevelElementParser >>= (fun expr -> preturn (op, expr))))
                     [])
                   >>= (fun acc -> preturn (buildExpressions leftTerm acc) ))


   let unaryExpressions = ref []

   let pUnaryExpression =
       fun state -> (List.reduce disjParser !unaryExpressions) state
   let pNot = pNotOperator >>
              pUnaryExpression >>= (fun expr ->
              preturn (PNot expr))

   let pArrayAccess =
       pSymbol >>= (fun symbol ->
       pLSquareBracket >>
       pExpression >>= (fun indexExpression ->
       pRSquareBracket >>
       (preturn (PArrayAccess(symbol, indexExpression)))))

   unaryExpressions := [ pNot; pCall; pArrayAccess; pPrimaryExpression]


   let pMultiplicativeExpression = pBinaryExpression (disjParser pDivOperator pTimesOperator)  pUnaryExpression
         
   let pAdditiveExpression = pBinaryExpression (disjParser plusOperator minusOperator)  pMultiplicativeExpression

   let pRelationalExpression = pBinaryExpression (disjParser pGtOperator pLtOperator) pAdditiveExpression

   let pEqualityExpression = pBinaryExpression (disjParser pEqualOperator pNotEqualOperator) pRelationalExpression

   let pLogicalOrExpression = pBinaryExpression pOrOperator pEqualityExpression

   let pLogicalAndExpression = pBinaryExpression pAndOperator pLogicalOrExpression
   

   pTopExpressions := [pLogicalAndExpression]

   let pReturn  = returnKeyword >>
                  pExpression >>=
                      (fun expr -> preturn (PReturn expr))
   let pCallStatement = pCall >>= fun call -> (preturn (PCallStat call))

   let pAssignStatement =
       pSymbol >>= (fun leftSide ->
       pAssignOperator >>
       pExpression >>= (fun rightSide ->
       preturn (PAssignStat(leftSide, rightSide))))
                                      

   let pStatements = ref []

   let pStatement =
       fun state -> (List.reduce disjParser !pStatements) state


   let emptyLine = newline

   let indentation =
          pGetIndentation >>=(fun indentation ->                          
                (readZeroOrMoreChars (fun c -> c = ' '))
                >>=
                (fun spaces ->
                   match (spaces.Length,
                          indentation) with
                   | (lessThan, top::_) when lessThan > top ->
                       (pSetIndentation lessThan) >> (preturn "INDENT")
                   | (lessThan, top::_) when lessThan = top ->
                       preturn "INDENTED"
                   | (identifiedIndentation, top::rest) ->
                          (pSetFullIndentation rest) >> (preturn "DEDENT")
                   | _ -> pfail))

   let indent = indentation >>= (fun result -> if result = "INDENT" then preturn result else pfail)
   let dedent = indentation >>= (fun result -> if result = "DEDENT" then preturn result else pfail)
   let indented = indentation >>= (fun result -> if result = "INDENTED" then preturn result else pfail)

   let newlines = oneOrMore newline []
   
   let pBlock =
       newlines   >>
       indent     >>
       pStatement >>= (fun firstStat ->
       (zeroOrMore
           (newlines  >>
            indented  >>
            pStatement) [])  >>= (fun restStats ->
       dedent      >> preturn (firstStat::restStats)))

   let pElse =
       newlines >>
       indented    >> 
       elseKeyword >>
       colon       >>
       pBlock


   let ifParser =
       ifKeyword   >>
       pExpression >>= (fun expr ->
       colon       >>
       pBlock      >>= (fun block ->
       (optionalP pElse []) >>= (fun optElseBlockStats ->
                     preturn (PIf(expr,
                                  block,
                                  (match optElseBlockStats with
                                   | [] -> None
                                   | elements -> Some elements))))))
             

   pStatements := [pReturn; ifParser; pCallStatement; pAssignStatement]

   let parse (input : string) (parser : (ReaderState ->  (('a * ReaderState) option ))) =
       parser {  Data = input ; Position = 0; Indentation = [0] }
