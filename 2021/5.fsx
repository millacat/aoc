open IOz
open System.Text.RegularExpressions
open System.Collections.Generic

type Point = int * int
type Line  = Point * Point

let (|ParseRegex|_|) regex s =
    let m = Regex.Match(s, regex)
    if m.Success
    then Some <| List.tail [ for x in m.Groups -> x.Value ]
    else None

let (|Integer|_|) (s : string) =
    let mutable intVal = 0
    if System.Int32.TryParse(s, &intVal)
    then Some intVal
    else None

let (|Line|_|) (s : string) = // : option<Line> =
    match s with
    | ParseRegex "(\d+),(\d+)\s+->\s+(\d+),(\d+)"
                 [Integer x1; Integer y1; Integer x2; Integer y2]
        -> Some <| ((x1, y1), (x2, y2))
    | _ -> None

let getLine = function
    | Line line -> line
    | _ -> failwith "Bad input data"



let inc (dict : Dictionary<Point, int>) (p : Point) =
    if dict.ContainsKey p
    then dict.[p] <- dict.[p] + 1 // increment value
    else dict.[p] <- 1            // initialize key, value pair

let initDictionary data =
    let dict = new Dictionary<Point,int>(HashIdentity.Structural)

    let getMinMax a b = (max a b, min a b)

    let rec addToDict dict lines =
        match lines with
        | [] -> dict
        | ((x1,y1),(x2,y2))::ls ->
            if x1 = x2 then     // horizontal points
                let max', min' = getMinMax y1 y2
                [min' .. max']
                    |> List.iter (fun y -> inc dict (x1,y))
            elif y1 = y2 then   // vertical points
                let max', min' = getMinMax x1 x2
                [min' .. max']
                    |> List.iter (fun x -> inc dict (x,y1))
            else                // diagonal points (* Part 2 ^.^ *)
                let xs = if x1 < x2 then [x1..x2] else [x2..x1] |> List.rev
                let ys = if y1 < y2 then [y1..y2] else [y2..y1] |> List.rev
                in List.iter2 (fun x y -> inc dict (x,y) ) xs ys

            addToDict dict ls

    addToDict dict data

let lines   = getData "5.txt" getLine
let dict    = initDictionary lines
let clashes = Seq.toList dict.Values |> List.filter (fun x -> x > 1)
clashes.Length |> prn
