open System

let prn n = printfn $"%A{n}"
let input = IO.File.ReadLines "input"

let findFirst s = Seq.find Char.IsDigit s
let findLast s = Seq.findBack Char.IsDigit s
let getCalibrationValue s = 
    $"{findFirst s}{findLast s}"
    |> int

let day1_1 =
    input
    |> Seq.sumBy getCalibrationValue
    |> prn

let findFirstIndex s = Seq.findIndex Char.IsDigit s
let findLastIndex s = Seq.findIndexBack Char.IsDigit s
let getIndex index (s: string) = s[index]
let strTodigit = function
    | "one" -> 1
    | "two" -> 2
    | "three" -> 3
    | "four" -> 4
    | "five" -> 5
    | "six" -> 6
    | "seven" -> 7
    | "eight" -> 8
    | "nine" -> 9
    | _ -> failwith "Should not happen. :)"

let digits = [
    "one"
    "two"
    "three"
    "four"
    "five"
    "six"
    "seven"
    "eight"
    "nine"
]

let getCalibrationValue2 (s : string) =
    //let indexed = List.indexed >> Seq.toList
    digits
    |> List.map (fun digit -> digit, s.IndexOf digit, s.LastIndexOf digit)
    //|> List.indexed
    //|> List.unzip
    //|> (fun (firsts, lasts) -> ())

let mutable count = 0
let getCalibrationValue3 (s : string) =
        let digitsInString = digits |> List.filter (fun digit -> s.IndexOf digit > 0 )
        prn digitsInString
        count <- count+1
        prn count
        let firsts = digitsInString |> List.map (fun digit -> digit, s.IndexOf digit )
        let lasts = digitsInString |> List.map (fun digit -> digit, s.LastIndexOf digit)
        let first,_ = firsts |> List.minBy snd
        let last,_ = lasts |> List.maxBy snd
        $"{ first |> strTodigit }{ last |> strTodigit }"
        |> int

let head = input |> Seq.head

let day1_2 =
    head |> prn

    head
    |> getCalibrationValue3
    |> prn

    "248onefouronexcjbbqpcfb"
    |> getCalibrationValue3
    |> prn

    // head
    // |> prn

    IO.File.ReadLines "input"
    |> Seq.sumBy getCalibrationValue3
    |> prn

day1_1
day1_2