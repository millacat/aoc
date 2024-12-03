open System
open System.Text.RegularExpressions

let input = @"c:\Users\cami\Code\aoc\2024\3\input"
let readFile path = IO.File.ReadAllLines path
let prn msg value = printfn $"{msg}: {value}"

let data = input |> readFile
prn "The input data" data // 6 lines

(* Day 3 - Part 1 - YAY! *)

// Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing.
// xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5)) has 4 operations

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) 
   then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let rec parseMultipliers acc = function
    | ParseRegex "mul\((\d{1,3}),(\d{1,3})\)(.*)" [ Integer x; Integer y; rest ] -> 
        parseMultipliers (x * y + acc) rest
    | _ -> acc

data
|> Array.map ( parseMultipliers 0 )
|> Array.sum
|> prn "Day 3, first part! MEEERRY CHRISTMASSY"

(* Day 3 - Part 2 - WOW! *)

// The do() instruction enables future mul instructions.
// The don't() instruction disables future mul instructions.

// let parseDo = function
//     | ParseRegex "do\(\)" [ rest ] -> true, rest

// let parseDont = function
//     | ParseRegex "don't\(\)" [ rest ] -> false, rest



// WORK IN PROGRESS. DOES NOT WORK.
let rec parseInstructionsMultipliers enabled acc input =
    match input with
    | ParseRegex "do\(\)(.*)" [ rest ] -> 
        printfn $"match do()"
        parseInstructionsMultipliers true 10 rest
    | ParseRegex "don't\(\)(.*)" [ rest ] ->
        printfn $"match don't()"
        parseInstructionsMultipliers true 20 rest

    | ParseRegex "mul\((\d{1,3}),(\d{1,3})\)" [ Integer x; Integer y; rest ] -> 
        printfn $"x: {x}, y: {y}"
        parseInstructionsMultipliers enabled (x * y + acc) rest
    | _ -> acc

data
|> Array.map (parseInstructionsMultipliers true 0)
|> Array.iter (fun i -> printfn $"i: {i}")
//|> Array.sum
|> prn "What I got"

