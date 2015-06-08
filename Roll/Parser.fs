// Implementation file for parser generated by fsyacc
module Parser
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "Parser.fsy"




# 11 "Parser.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | QuestionMark
  | CloseParen
  | OpenParen
  | Comma
  | Disadvantage
  | Advantage
  | Max
  | Min
  | Dot
  | Minus
  | Plus
  | DSymbol
  | Number of (int)
  | EOF
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_QuestionMark
    | TOKEN_CloseParen
    | TOKEN_OpenParen
    | TOKEN_Comma
    | TOKEN_Disadvantage
    | TOKEN_Advantage
    | TOKEN_Max
    | TOKEN_Min
    | TOKEN_Dot
    | TOKEN_Minus
    | TOKEN_Plus
    | TOKEN_DSymbol
    | TOKEN_Number
    | TOKEN_EOF
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_MultiRoll
    | NONTERM_Roll
    | NONTERM_RollList
    | NONTERM_PlusList
    | NONTERM_SimpleRoll

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | QuestionMark  -> 0 
  | CloseParen  -> 1 
  | OpenParen  -> 2 
  | Comma  -> 3 
  | Disadvantage  -> 4 
  | Advantage  -> 5 
  | Max  -> 6 
  | Min  -> 7 
  | Dot  -> 8 
  | Minus  -> 9 
  | Plus  -> 10 
  | DSymbol  -> 11 
  | Number _ -> 12 
  | EOF  -> 13 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_QuestionMark 
  | 1 -> TOKEN_CloseParen 
  | 2 -> TOKEN_OpenParen 
  | 3 -> TOKEN_Comma 
  | 4 -> TOKEN_Disadvantage 
  | 5 -> TOKEN_Advantage 
  | 6 -> TOKEN_Max 
  | 7 -> TOKEN_Min 
  | 8 -> TOKEN_Dot 
  | 9 -> TOKEN_Minus 
  | 10 -> TOKEN_Plus 
  | 11 -> TOKEN_DSymbol 
  | 12 -> TOKEN_Number 
  | 13 -> TOKEN_EOF 
  | 16 -> TOKEN_end_of_input
  | 14 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startstart 
    | 1 -> NONTERM_start 
    | 2 -> NONTERM_MultiRoll 
    | 3 -> NONTERM_MultiRoll 
    | 4 -> NONTERM_Roll 
    | 5 -> NONTERM_Roll 
    | 6 -> NONTERM_Roll 
    | 7 -> NONTERM_Roll 
    | 8 -> NONTERM_RollList 
    | 9 -> NONTERM_RollList 
    | 10 -> NONTERM_PlusList 
    | 11 -> NONTERM_PlusList 
    | 12 -> NONTERM_PlusList 
    | 13 -> NONTERM_SimpleRoll 
    | 14 -> NONTERM_SimpleRoll 
    | 15 -> NONTERM_SimpleRoll 
    | 16 -> NONTERM_SimpleRoll 
    | 17 -> NONTERM_SimpleRoll 
    | 18 -> NONTERM_SimpleRoll 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 16 
let _fsyacc_tagOfErrorTerminal = 14

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | QuestionMark  -> "QuestionMark" 
  | CloseParen  -> "CloseParen" 
  | OpenParen  -> "OpenParen" 
  | Comma  -> "Comma" 
  | Disadvantage  -> "Disadvantage" 
  | Advantage  -> "Advantage" 
  | Max  -> "Max" 
  | Min  -> "Min" 
  | Dot  -> "Dot" 
  | Minus  -> "Minus" 
  | Plus  -> "Plus" 
  | DSymbol  -> "DSymbol" 
  | Number _ -> "Number" 
  | EOF  -> "EOF" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | QuestionMark  -> (null : System.Object) 
  | CloseParen  -> (null : System.Object) 
  | OpenParen  -> (null : System.Object) 
  | Comma  -> (null : System.Object) 
  | Disadvantage  -> (null : System.Object) 
  | Advantage  -> (null : System.Object) 
  | Max  -> (null : System.Object) 
  | Min  -> (null : System.Object) 
  | Dot  -> (null : System.Object) 
  | Minus  -> (null : System.Object) 
  | Plus  -> (null : System.Object) 
  | DSymbol  -> (null : System.Object) 
  | Number _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
  | EOF  -> (null : System.Object) 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; 1us; 65535us; 0us; 2us; 2us; 65535us; 0us; 7us; 5us; 6us; 3us; 65535us; 9us; 10us; 11us; 12us; 16us; 17us; 4us; 65535us; 0us; 8us; 5us; 8us; 19us; 20us; 21us; 22us; 7us; 65535us; 0us; 18us; 5us; 18us; 9us; 15us; 11us; 15us; 16us; 15us; 19us; 18us; 21us; 18us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; 3us; 5us; 8us; 12us; 17us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; 1us; 1us; 4us; 2us; 13us; 14us; 15us; 1us; 2us; 2us; 2us; 7us; 2us; 3us; 7us; 1us; 4us; 1us; 5us; 1us; 5us; 1us; 6us; 1us; 6us; 1us; 7us; 1us; 7us; 4us; 8us; 9us; 17us; 18us; 1us; 8us; 1us; 8us; 5us; 10us; 11us; 12us; 17us; 18us; 1us; 11us; 1us; 11us; 1us; 12us; 1us; 12us; 3us; 13us; 14us; 15us; 2us; 14us; 15us; 1us; 15us; 1us; 16us; 1us; 16us; 1us; 17us; 1us; 18us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; 6us; 8us; 13us; 15us; 18us; 21us; 23us; 25us; 27us; 29us; 31us; 33us; 35us; 40us; 42us; 44us; 50us; 52us; 54us; 56us; 58us; 62us; 65us; 67us; 69us; 71us; 73us; |]
let _fsyacc_action_rows = 30
let _fsyacc_actionTableElements = [|4us; 32768us; 6us; 11us; 7us; 9us; 11us; 26us; 12us; 4us; 0us; 49152us; 1us; 32768us; 13us; 3us; 0us; 16385us; 2us; 16397us; 8us; 5us; 11us; 24us; 4us; 32768us; 6us; 11us; 7us; 9us; 11us; 26us; 12us; 23us; 1us; 16386us; 0us; 13us; 1us; 16387us; 0us; 13us; 0us; 16388us; 2us; 32768us; 11us; 26us; 12us; 23us; 0us; 16389us; 2us; 32768us; 11us; 26us; 12us; 23us; 0us; 16390us; 1us; 32768us; 12us; 14us; 0us; 16391us; 3us; 16393us; 3us; 16us; 4us; 29us; 5us; 28us; 2us; 32768us; 11us; 26us; 12us; 23us; 0us; 16392us; 4us; 16394us; 4us; 29us; 5us; 28us; 9us; 21us; 10us; 19us; 2us; 32768us; 11us; 26us; 12us; 23us; 0us; 16395us; 2us; 32768us; 11us; 26us; 12us; 23us; 0us; 16396us; 1us; 16397us; 11us; 24us; 1us; 16398us; 12us; 25us; 0us; 16399us; 1us; 32768us; 12us; 27us; 0us; 16400us; 0us; 16401us; 0us; 16402us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 5us; 6us; 8us; 9us; 12us; 17us; 19us; 21us; 22us; 25us; 26us; 29us; 30us; 32us; 33us; 37us; 40us; 41us; 46us; 49us; 50us; 53us; 54us; 56us; 58us; 59us; 61us; 62us; 63us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 2us; 3us; 1us; 1us; 2us; 2us; 3us; 3us; 1us; 1us; 3us; 3us; 1us; 2us; 3us; 2us; 2us; 2us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; 2us; 2us; 3us; 3us; 3us; 3us; 4us; 4us; 5us; 5us; 5us; 6us; 6us; 6us; 6us; 6us; 6us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 65535us; 16385us; 65535us; 65535us; 65535us; 65535us; 16388us; 65535us; 16389us; 65535us; 16390us; 65535us; 16391us; 65535us; 65535us; 16392us; 65535us; 65535us; 16395us; 65535us; 16396us; 65535us; 65535us; 16399us; 65535us; 16400us; 16401us; 16402us; |]
let _fsyacc_reductions ()  =    [| 
# 168 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data :  Statements.RollSpec )) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startstart));
# 177 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'MultiRoll)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 34 "Parser.fsy"
                                            _1 
                   )
# 34 "Parser.fsy"
                 :  Statements.RollSpec ));
# 188 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'Roll)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 37 "Parser.fsy"
                                          Statements.Repeat(_1, _3) 
                   )
# 37 "Parser.fsy"
                 : 'MultiRoll));
# 200 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Roll)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 38 "Parser.fsy"
                               _1 
                   )
# 38 "Parser.fsy"
                 : 'MultiRoll));
# 211 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'PlusList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 41 "Parser.fsy"
                                   _1 
                   )
# 41 "Parser.fsy"
                 : 'Roll));
# 222 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'RollList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 42 "Parser.fsy"
                                       Statements.Min(_2) 
                   )
# 42 "Parser.fsy"
                 : 'Roll));
# 233 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : 'RollList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 43 "Parser.fsy"
                                       Statements.Max(_2) 
                   )
# 43 "Parser.fsy"
                 : 'Roll));
# 244 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'Roll)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 44 "Parser.fsy"
                                                   Statements.AtLeast(_1, _3) 
                   )
# 44 "Parser.fsy"
                 : 'Roll));
# 256 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'SimpleRoll)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'RollList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 47 "Parser.fsy"
                                                    List.append [_1] _3 
                   )
# 47 "Parser.fsy"
                 : 'RollList));
# 268 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'SimpleRoll)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 48 "Parser.fsy"
                                     [_1] 
                   )
# 48 "Parser.fsy"
                 : 'RollList));
# 279 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'SimpleRoll)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 51 "Parser.fsy"
                                     _1 
                   )
# 51 "Parser.fsy"
                 : 'PlusList));
# 290 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'SimpleRoll)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'PlusList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 52 "Parser.fsy"
                                                   Statements.MakeSum(_1, _3) 
                   )
# 52 "Parser.fsy"
                 : 'PlusList));
# 302 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'SimpleRoll)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : 'PlusList)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 53 "Parser.fsy"
                                                    Statements.MakeSubtract(_1, _3) 
                   )
# 53 "Parser.fsy"
                 : 'PlusList));
# 314 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 56 "Parser.fsy"
                                      Statements.Roll(0, 0, _1) 
                   )
# 56 "Parser.fsy"
                 : 'SimpleRoll));
# 325 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 57 "Parser.fsy"
                                            Statements.Roll(_1, 6, 0) 
                   )
# 57 "Parser.fsy"
                 : 'SimpleRoll));
# 336 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            let _3 = (let data = parseState.GetInput(3) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 58 "Parser.fsy"
                                                  Statements.Roll(_1, _3, 0) 
                   )
# 58 "Parser.fsy"
                 : 'SimpleRoll));
# 348 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _2 = (let data = parseState.GetInput(2) in (Microsoft.FSharp.Core.Operators.unbox data : int)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 59 "Parser.fsy"
                                                Statements.Roll(1, _2, 0) 
                   )
# 59 "Parser.fsy"
                 : 'SimpleRoll));
# 359 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'SimpleRoll)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 60 "Parser.fsy"
                                                   Statements.Max [_1; _1] 
                   )
# 60 "Parser.fsy"
                 : 'SimpleRoll));
# 370 "Parser.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : 'SimpleRoll)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 61 "Parser.fsy"
                                                     Statements.Min [_1; _1] 
                   )
# 61 "Parser.fsy"
                 : 'SimpleRoll));
|]
# 382 "Parser.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 17;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let start lexer lexbuf :  Statements.RollSpec  =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))
