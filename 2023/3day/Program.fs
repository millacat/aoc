
open System
open System.Text.RegularExpressions

let prn n = printfn $"%A{n}"
let input = System.IO.File.ReadLines "input" |> Array.ofSeq
let test = System.IO.File.ReadLines "test" |> Array.ofSeq

type Position = int * int
type Positions = Position list

let SYMBOLS = "@*-_+$/&%=#!^"
let isSymbol (c : char) = SYMBOLS.Contains c && c <> '.'

let getSymbolPositions input : Positions =
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

let symbolPositions = input |> getSymbolPositions

let left, right = (0, -1), (0, 1)
let up, down = (-1, 0), (1, 0)
let upleft, upright = (-1, -1), (-1, 1)
let downleft, downright = (1, -1), (1, 1)
let above = [upleft; up; upright;]
let below = [downleft; down; downright]
let beside = [left; right]

let addPositions (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let positionHasDigit (data : string[]) (x, y) = data[x][y] |> Char.IsDigit

let maybeGetDigit (data: string[]) (x, y) =
    let c = data[x][y]
    in if Char.IsDigit c then Some (c |> string |> int) else None

type Direction = Left | Right

let search data startPosition direction = 
    let tryGetDigit = maybeGetDigit data
    let rec search' depth ((x,y) as pos) =
        if depth = 0 
        then [] 
        else
            match tryGetDigit pos with
            | Some d -> 
                match direction with // put smallest part of number in front
                | Left -> d :: search' (depth - 1) (x, y - 1) 
                | Right -> search' (depth - 1) (x, y + 1) @ [d]
            | None -> []
    search' 3 startPosition

let digitListToNumber l = 
    let rec calc acc factor = function
        | [] -> acc
        | d :: drest -> calc (factor*d + acc) (factor*10) drest
    calc 0 1 l // utilise that smallest part of number is in front of list
    
let areNeighbors (_,y1) (_,y2) = y2-y1 = 1
let isNonEmpty l = List.isEmpty l |> not

let getSum (data:string[]) symPositions directions : int =
    let getPositionsToCheck symPos = List.map (addPositions symPos) directions
    let keepPositionWithDigit = List.filter (positionHasDigit data)
    let search' = fun p dir -> search data p dir |> digitListToNumber
    let keepBiggestFoundNumber p1 p2 =
        let leftDigit = search' p1 Left
        let rightDigit = search' p2 Right
        if leftDigit > rightDigit then leftDigit else rightDigit
    let buildNumbers = function
        | [p] -> keepBiggestFoundNumber p p 
        | [p1; p2] -> 
            if areNeighbors p1 p2 
            then keepBiggestFoundNumber p2 p1 // keep biggest number
            else search' p1 Left + search' p2 Right // keep both numbers
        | [p1; _; _] -> search' p1 Right
        | _ -> failwith "Should not happen"
    
    let getNumbersInDirections directions =
            symPositions
            |> List.map getPositionsToCheck
            |> List.map keepPositionWithDigit
            |> List.filter isNonEmpty
            |> List.map buildNumbers
            |> List.sum
    getNumbersInDirections directions

prn "Day 3 - Part 1"
[above; below; beside]
|> List.sumBy (getSum input symbolPositions)
|> prn

