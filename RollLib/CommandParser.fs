namespace RollLib
module CommandParser =
    open RollLib.Statements

    (* Throughout this file we use string * int as a data type for parse inputs,
    representing a string and a position within it. I can't figure out how to get
    the type inference to work with type aliases though so I'm just using a raw
    string * int tuple. Wherever you see "string * int" in a type, think "parser
    input."
    *)

    let alpha = set['A'..'Z'] + set['a'..'z']
    let numeric = set['0'..'9']
    let alphanumeric = alpha + numeric

    let (|Next|Empty|) = function
        | (input : string), pos when pos < input.Length -> Next(input.[pos], (input, pos+1))
        | _ -> Empty

    let (|Char|_|) alphabet = function
      | Empty -> None
      | s, i when Set.contains s.[i] alphabet -> Some(s, i+1)
      | _ -> None

    let rec (|MaybeChars|) alphabet = function
      | Char alphabet (MaybeChars alphabet it)
      | it -> it

    let rec (|Chars|_|) alphabet = function
      | Char alphabet (MaybeChars alphabet it) -> Some it
      | it -> None

    let sub (s: string, i0) (_, i1) =
      s.Substring(i0, i1-i0)

    let rec (|RollCommand|_|) = function
        | Repeat (expr, Empty) -> Some (Statements.RollCommand expr)
        | _ -> None
    and (|Repeat|_|) = function
        | Number (repeats, (Next('.', DiceExpression(expr, next)))) -> Some (Statements.Repeat(repeats, Statements.Simple expr), next)
        | DiceExpression(expr, next) -> Some (Statements.Simple expr, next)
        | _ -> None
    and (|DiceExpression|_|) = function
        | Next('d', Number(dieSize, next)) -> Some (Statements.Roll(1, dieSize, 0), next)
        | Number (n, Next('d', Number(dieSize, next))) -> Some (Statements.Roll(n, dieSize, 0), next)
        | _ -> None
    and (|Number|_|) = function
        | Chars numeric i1 as i0 -> (System.Int32.Parse(sub i0 i1), i1) |> Some
        | _ -> None

    let parse txt =
        match (txt, 0) with
        | RollCommand(cmd) -> cmd
        | _ -> failwithf "failed to parse '%s'" txt

