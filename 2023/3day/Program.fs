
open System
open System.Text.RegularExpressions

let prn n = printfn $"%A{n}"
let input = System.IO.File.ReadLines "input" |> Array.ofSeq
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

let left, right = (0, -1), (0, 1)
let up, down = (-1, 0), (1, 0)
let upleft, upright = (-1, -1), (-1, 1)
let downleft, downright = (1, -1), (1, 1)
let directions = [left; right; up; down; upleft; upright; downleft; downright]

let addPositions (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let positionHasDigit (x, y) = input[x][y] |> Char.IsDigit

let positionsToCheck = 
    directions 
    |> List.map (addPositions syms[0]) 
positionsToCheck |> prn

let positionsWithDigit = positionsToCheck |> List.filter positionHasDigit
positionsWithDigit |> prn

input[0][22] |> prn
let firstLine = input[0]
let n = lineLength - (lineLength-22)
prn (lineLength, n)

let offset = 2
let potential = firstLine[n-offset .. n+offset]
let getNumber potential = let re = Regex @"\d+" in re.Match potential |> _.Value |> Int32.Parse
prn (potential, firstLine)
getNumber potential
|> prn
