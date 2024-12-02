let input = @"c:\Users\cami\Code\aoc\2024\1\input"
let readFile path = System.IO.File.ReadAllLines path
let prn msg value = printfn $"{msg}: {value}"
let parseLine (l : string) =
    let fst = int l[..4]
    let snd = int l[7..]
    fst, snd

let data = 
    input 
    |> readFile
    |> Array.map parseLine
    |> Array.unzip

let (col1,col2) as sorted =
    data
    |> fun (col1, col2) -> Array.sort col1, Array.sort col2

(* Day 1.1 - Merry Christmas! *)
let result1 =
    sorted
    ||> Array.fold2 (fun sum i1 i2 -> i1 - i2 |> (System.Math.Abs >> (+) sum)) 0

printfn $"Day 1, first result: {result1}"

(* Day 1.2 - And a Happy Holiday! *)
let col2count =
    col2
    |> Array.countBy id

let result2 =
    col1
    |> Array.fold (fun sum i1 -> 
            col2count 
            |> Array.tryFind (fun (i2, _count) -> i1 = i2)
            |> function | None -> sum | Some (_i2, count) -> i1 * count + sum
        ) 0

printfn $"Day 1, second result: {result2}"

