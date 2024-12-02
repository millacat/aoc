open System

let input = @"c:\Users\cami\Code\aoc\2024\2\input"
let readFile path = IO.File.ReadAllLines path
let prn msg value = printfn $"{msg}: {value}"

let data = input |> readFile
prn "The input data" data


(* Day 2 - Part 1 - Very Merry Christmassis! *)

let parseReport (s:string) =
    s.Split ' '
    |> Array.map int
    |> List.ofArray

let reports =
    data
    |> List.ofArray
    |> List.map parseReport

let rec safe f comp = function
    | []
    | [_] -> true
    | l :: l' :: ls -> 
        let d = l - l' |> f
        comp l l' && 1 <= d && d <= 3 && safe f comp (l' :: ls)

let safeDecrease = safe id (>)
let safeIncrease = safe Math.Abs (<)
let isSafe report = safeDecrease report || safeIncrease report

let result1 = 
    reports
    |> List.filter isSafe
    |> List.length

printfn $"Day 2, first result: {result1}"

(* Day 2 - Part 2 - Very Merry Christmassis! *)
let flip f x y = f y x
let result2 = 
    reports
    |> List.map (fun report -> report :: ([0..report.Length - 1] |> List.map (flip List.removeAt report)))
    |> List.map (List.exists isSafe)
    |> List.filter id
    |> List.length


// Test Part 1
let inc = [1;2;3;5;8]
let incSame = [1;3;4;4;6]
let incDec = [1; 3; 2; 5]
let dec = [8; 5; 2; 1]
let decSame = [8; 5; 2; 2]
let decInc = [9; 6; 6; 3]

let testReports = [inc; incSame; incDec; dec; decSame; decInc]
let expected = [true; false; false; true; false; false]

let r =
    testReports
    |> List.map isSafe
    |> List.iter2 (fun r exp -> printfn $"result: {r}, expected: {exp}") expected