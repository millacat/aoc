
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
    |> Array.fold
        (fun positions (xIndex, row) -> 
            let yIndices = 
                row 
                |> Seq.indexed
                |> Seq.filter (snd >> isSymbol)
                |> Seq.map fst
                |> List.ofSeq
            let newPositions = 
                ( List.replicate yIndices.Length xIndex
                , yIndices ) 
                ||> List.zip
            positions @ newPositions) []


let syms = 
    input
    |> getSymbolPositions

syms |> prn
syms[0] |> prn

let left, right = (0, -1), (0, 1)
let up, down = (-1, 0), (1, 0)
let upleft, upright = (-1, -1), (-1, 1)
let downleft, downright = (1, -1), (1, 1)
let directions = [left; right; up; down; upleft; upright; downleft; downright]
prn directions

let addPositions (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let positionHasNumber (x, y) = input[x][y] |> Char.IsDigit

positionHasNumber (0, 11) |> prn

let positionsToCheck = 
    directions 
    |> List.map (addPositions syms[0]) 
positionsToCheck |> prn

let positionsWithNumber = positionsToCheck |> List.filter positionHasNumber
positionsWithNumber |> prn
input[0][22] |> prn
let l0 = input[0]
let n = lineLength - (lineLength-22)
prn (lineLength, n)

let sizes = [1, 2]

let r = l0[n .. n + 2]

// https://stackoverflow.com/questions/2633220/find-a-string-within-another-string-search-backwards