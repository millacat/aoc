open IOz
open System.IO

let parseData file =
    File.ReadAllText file
    |> (fun s -> s.Split ',')
    |> Array.map int
    |> List.ofArray

let data = parseData "7.txt" |> List.sort

let part1 (data : int list) =
    let median = data.Length / 2 |> (fun i -> data.[i])
    List.fold (fun cost p -> median - p |> System.Math.Abs |> (+) cost ) 0 data

part1 data |> prn

let part2 (data : int list) =
    let fdata = List.map float data
    let mean  = List.average fdata |> System.Math.Floor
    let diff  = List.map (fun p -> mean - p |> System.Math.Abs |> int) fdata
    List.fold (fun acc d -> [0..d] |> List.sum |> (+) acc) 0 diff

part2 data |> prn

