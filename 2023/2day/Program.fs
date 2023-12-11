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
        | ParseRegex "Game\s+(\d+):\s+(.+)" [Integer id; Sets sets] ->
            let rounds = sets.Split ";" |> List.ofArray
            in Some (id, rounds)
        | _ -> None

    let getGameLine = function
        | Line line -> line
        | _ -> failwith "Bad input data"

    let parseColor s =
        match s with
        | ParseRegex "(\d+)\s+(red|blue|green)" [Integer n; Color color] -> Some (n, color)
        | _ -> None

module part1 =

    let toGame id drawSets = {
        id = id
        sets = drawSets |> List.map (List.map Option.get)
    }

    let getAmountOfColors (set : string) = 
        set.Split ", "
        |> List.ofArray 
        |> List.map parse.parseColor

    let gameRespectsLimit game = 
        let colorRespectsLimit (n, color) =
            match color with
            | Red -> n <= 12
            | Green -> n <= 13
            | Blue -> n <= 14
        game.sets |> List.forall (List.forall colorRespectsLimit)

    let getIdIfGameRespectsLimit (id, sets) =
        sets
        |> List.map getAmountOfColors
        |> toGame id
        |> gameRespectsLimit
        |> fun respect -> if respect then id else 0

    let execute =
        input
        |> List.map parse.getGameLine
        |> List.map getIdIfGameRespectsLimit
        |> List.sum

part1.execute |> prn