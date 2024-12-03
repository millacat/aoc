open System
open System.Text.RegularExpressions

let input = "input"
let readFile path = IO.File.ReadAllLines path
let prn msg value = printfn $"{msg}: {value}"
let data = input |> readFile

(* Day 3 - Part 1 - YAY! *)

// Sequences like mul(4*, mul(6,9!, ?(12,34), or mul ( 2 , 4 ) do nothing
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

let rec parseMultipliers keep input =
    match input with
    | ParseRegex "mul\((\d{1,3}),(\d{1,3})\)(.*)" [ Integer x; Integer y; rest ] -> 
        parseMultipliers (x * y + keep) rest
    | _ -> keep

data
|> Array.map ( parseMultipliers 0 )
|> Array.sum
|> prn "Day 3, first part! MEEERRY CHRISTMASSY"

(* Day 3 - Part 2 - WOW! *)

// The do() instruction enables future mul instructions
// The don't() instruction disables future mul instructions

let rec parseDont keep = function
    | ParseRegex "(.*?)don't\\(\\)(.*)" [ beforeDont; afterDont ] -> 
        parseDo (keep + beforeDont) afterDont
    | input -> keep + input

and parseDo keep = function
    | ParseRegex "(.*?)do\(\)(.*)" [ _betweenDontAndDo; afterDo] ->
        parseDont keep afterDo
    | _ -> keep

data
|> Array.fold (+) ""
|> parseDont ""
|> parseMultipliers 0
|> prn "Day 3, second part! Happy Holidays, peeps"


(* Tests part 2 *)
"912734don't()12938037do()109hsaiy21sakdon't()1203712078do()..12431"
|> parseDont ""
|> prn "\n"

"912734don't()12938037do()109hsaiy21sakdon't()1203712078do()..1don't()2431"
|> parseDont ""
|> prn ""

