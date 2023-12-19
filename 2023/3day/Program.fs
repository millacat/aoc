
open System
open System.Text.RegularExpressions

let prn n = printfn $"%A{n}"
let input = System.IO.File.ReadLines "input" |> Array.ofSeq
let test = System.IO.File.ReadLines "test" |> Array.ofSeq

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


let symbolPositions = 
    input
    |> getSymbolPositions

//symbolPositions |> prn

let left, right = (0, -1), (0, 1)
let up, down = (-1, 0), (1, 0)
let upleft, upright = (-1, -1), (-1, 1)
let downleft, downright = (1, -1), (1, 1)
let directions = [upleft; up; upright; left; right; downleft; down; downright]
let above = [upleft; up; upright;]
let below = [downleft, down, downright]
let beside = [left, right]

let addPositions (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let positionHasDigit (data : string[]) (x, y) = data[x][y] |> Char.IsDigit

// let positionsToCheck' = 
//     directions 
//     |> List.map (addPositions symbolPositions[0]) 
// //positionsToCheck' |> prn


let positionsToCheck =
    symbolPositions
    |> List.collect (fun sp -> directions |> List.map (addPositions sp))

// ("POSITIONS TO CHECK\n", positionsToCheck.Length,
// positionsToCheck) |> prn

let getPositionsWithDigit (data:string[]) posToCheck = 
    posToCheck 
    |> List.filter (positionHasDigit data)
    |> List.groupBy fst

let offset = 2
// let containsNumber = firstLine[n - offset .. n + offset]
let getNumber (digit : int) containsNumber = 
    let re = Regex $@"\d*{digit}\d*" in re.Match containsNumber |> _.Value |> int

"test" |> prn
test |> Array.iter prn
printfn ""

let symPos = test |> getSymbolPositions
prn "Symbol positions"
symPos |> prn 
let positionsToCheck2 = symPos |> List.collect (fun sp -> directions |> List.map (addPositions sp))
//positionsToCheck2 |> prn

let getPosWDig (data:string[]) symPositions directions =
    let check directions =
        symPositions
        |> List.map (fun symPos -> List.map (addPositions symPos) directions)
        |> List.map (List.filter (positionHasDigit data))
        |> List.filter (fun l -> List.isEmpty l |> not) // only keep non-empty lists
        |> List.map (fun l -> if l.Length = directions.Length then [l.Head] else l) // if all three show up, only keep one
        // check if is two positions are neighbors and then only keep one.
        |> List.map (fun l -> if l.Length = 1 then l else // it can only be length 2 and then compare them
                    )
    check directions


let r = getPosWDig test symPos above
printfn "\nPositions ABOVE symbols with digits in test"
r |> prn

let posWithDigits = getPositionsWithDigit test positionsToCheck2
printfn "\n"
"Positions containing digits" |> prn
posWithDigits |> prn
//let posWithDigits2 = getPositionsWithDigit2 posWithDigits
// printfn "\n"
// "Filtered positions, so no positions right by each other" |> prn
//posWithDigits2 |> prn
(*  (2,3) og (2,2) ligger lige ved siden af hinanden, så de er en del af samme tal.
    (2,6) og (2,7) samme. 
    Fjern største, dvs (2,3) og (2,7).
    [(2, [(2, 3); (2, 2); (2, 6); (2, 7)]); 
     (0, [(0, 2)]); 
     (4, [(4, 2)]);
     (6, [(6, 4)]); 
     (9, [(9, 3); (9, 2); (9, 5); (9, 6)]); 
     (7, [(7, 6)])]
*)



// let getPositionsWithDigit2 inp =
//     inp |> List.map (fun (_,poss) ->
//         poss
//         |> List.fold (fun (i, acc) ((x,y1) as inp) ->
//             if poss.Length = 1
//             then (i, inp::acc)
//             else if i = poss.Length
//                  then (1111, acc)
//                  else
//                     let y2 : int = snd poss[i]
//                     prn ("poss[i]", poss[i], "y2", y2, "y1", y1, "mathabs",Math.Abs(y1-y2) )
//                     if Math.Abs(y1-y2) = 1
//                     then (i+1, inp :: acc)
//                     else (i+1, inp :: (x, y2) :: acc)
//             ) (1, [])
//     )

   // |> List.map (fun (x, ys) -> data[x], List.map snd ys)
// ("POSITIONS WITH DIGIT", positionsWithDigit.Length,
// positionsWithDigit) |> prn
//let n = 22
// input[0][n] |> prn
// let firstLine = input[0]

// prn (containsNumber, firstLine)
//getNumber containsNumber
//|> prn

// positionsToCheck
// |> getPositionsWithDigit input 
// |> List.map (fun (row, ys) ->
//     ys
//     |> List.sumBy (fun y ->
//         let digit = row[y] |> string |> int
//         let containsNumber = row[y - offset .. y + offset]
//         in getNumber digit containsNumber))
// |> List.sum
// |> ignore
//|> prn

