open System

let prn n = printfn $"%A{n}"
let input = IO.File.ReadLines "input"

module part1 =
    let findFirst s = Seq.find Char.IsDigit s |> string
    let findLast s = Seq.findBack Char.IsDigit s |> string
    let getCalibrationValue s = findFirst s + findLast s |> int

    let execute =
        input
        |> Seq.sumBy getCalibrationValue
        |> prn

module part2 =
    let strToInt = function
        | "one"   -> 1
        | "two"   -> 2
        | "three" -> 3
        | "four"  -> 4
        | "five"  -> 5
        | "six"   -> 6
        | "seven" -> 7
        | "eight" -> 8
        | "nine"  -> 9
        | _ -> failwith "Should not happen. :)"
    let digits = [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ]
    let findFirstNumberIndex s = Seq.findIndex Char.IsDigit s
    let findLastNumberIndex s = Seq.findIndexBack Char.IsDigit s

    let getCalibrationValue (s : string) =
        let containedDigits = digits |> List.filter s.Contains
        if containedDigits.IsEmpty
        then part1.getCalibrationValue s
        else
            let first,firstsIndex = containedDigits |> List.map (fun digit -> digit |> strToInt, s.IndexOf digit ) |> List.minBy snd
            let last,lastsIndex   = containedDigits |> List.map (fun digit -> digit |> strToInt, s.LastIndexOf digit) |> List.maxBy snd
            let tens =
                let index = findFirstNumberIndex s in
                if index < firstsIndex then part1.findFirst s |> int else first
            let ones =
                let index = findLastNumberIndex s in
                if index > lastsIndex then part1.findLast s |> int else last
            10*tens+ones
    let execute =
        input
        |> Seq.sumBy getCalibrationValue
        |> prn

part1.execute
part2.execute