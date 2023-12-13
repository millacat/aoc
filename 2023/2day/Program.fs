open System
open System.Text.RegularExpressions

let prn n = printfn $"%A{n}"
let input = IO.File.ReadLines "input" |> List.ofSeq

type Color = Red | Blue | Green
type Game = {
    id : int
    sets : (int * Color) list list
}

module parse =
    let (|ParseRegex|_|) regex s =
        let m = Regex.Match(s, regex)
        if m.Success
        then List.tail [ for x in m.Groups -> x.Value ] |> Some
        else None

    let (|Integer|_|) (s : string) =
        let mutable intVal = 0
        if System.Int32.TryParse(s, &intVal)
        then Some intVal
        else None

    let (|Color|_|) = function
        | "red" -> Some Red
        | "blue" -> Some Blue
        | "green" -> Some Green
        | _ -> None

    let (|Sets|) = id

    let (|Line|_|) (s : string) =
        match s with
        | ParseRegex "Game (\d+): (.+)" [Integer id; Sets sets] ->
            let rounds = sets.Split ";" |> List.ofArray
            in Some (id, rounds)
        | _ -> None

    let getGameLine = function
        | Line line -> line
        | _ -> failwith "Bad input data"
    
    let (|ParseColor|_|) (s : string) =
        match s with
        | ParseRegex "(\d+) (red|blue|green)" [Integer n; Color color] -> Some (n, color)
        | _ -> None

    let getAmountOfColor = function
        | ParseColor color -> color
        | _ -> failwith "Bad input data"

module part1 =
    let redLimit = 12
    let greenLimit = 13
    let blueLimit = 14

    let getAmountOfColors (set : string) = 
        set.Split ", "
        |> List.ofArray 
        |> List.map parse.getAmountOfColor

    let gameRespectsLimit cubeSets = 
        let colorRespectsLimit (n, color) =
            match color with
            | Red -> n <= redLimit
            | Green -> n <= greenLimit
            | Blue -> n <= blueLimit
        cubeSets |> List.forall (List.forall colorRespectsLimit)

    let getIdIfGameRespectsLimit (id, sets) =
        sets
        |> List.map getAmountOfColors
        |> gameRespectsLimit
        |> fun respect -> if respect then id else 0

    let execute =
        input
        |> List.map parse.getGameLine
        |> List.map getIdIfGameRespectsLimit
        |> List.sum

// Calculate sum of power of minimum sets allowing games
module part2 =
    let getMinimumNumberOfCubes cubeSets =
        ((0,0,0), cubeSets) 
        ||> List.fold (fun ((red,green,blue) as colors) (n, color) -> 
                    match color with
                    | Red -> if n > red then (n, green, blue) else colors
                    | Green -> if n > green then (red, n, blue) else colors
                    | Blue -> if n > blue then (red, green, n) else colors)

    let getSets = snd
    let sumOfPower = fun sum (red,green,blue) -> sum + red*green*blue

    let execute =
        input
        |> List.map (
            parse.getGameLine 
            >> getSets
            >> List.map part1.getAmountOfColors 
            >> List.concat 
            >> getMinimumNumberOfCubes
        )
        |> List.fold sumOfPower 0

part1.execute |> prn
part2.execute |> prn
