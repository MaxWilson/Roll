// Signature file for parser generated by fsyacc
module Parser
type token = 
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
type tokenId = 
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
type nonTerminalId = 
    | NONTERM__startstart
    | NONTERM_start
    | NONTERM_Roll
    | NONTERM_SimpleRoll
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val start : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> ( Rolls.RollSpec ) 
