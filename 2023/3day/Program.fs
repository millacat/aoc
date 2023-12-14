
open System

let prn n = printfn $"%A{n}"
let input = IO.File.ReadLines "input" |> Array.ofSeq
let lineLength = input[0].Length
type Point = { x : int; y : int }

type Symbols = Point list

let SYMBOLS = "@*-_+$/&%=#!^"
let isSymbol (c : char) = SYMBOLS.Contains c && c <> '.'

let getSymbolPositions input =
    input
    |> Array.indexed
    |> Array.fold (fun positions (xIndex, row) -> 
                        let yIndices = 
                            row 
                            |> Seq.indexed
                            |> Seq.filter (snd >> isSymbol)
                            |> Seq.map fst
                            |> List.ofSeq
                        let newPositions = 
                            (yIndices, List.init yIndices.Length (fun _ -> xIndex)) 
                            ||> List.zip                        // |> Array.zip (Array.map snd newSymbols)
                        positions @ newPositions) []


let syms = 
    input
    |> getSymbolPositions

syms |> prn
