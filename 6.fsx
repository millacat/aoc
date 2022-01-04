open IOz
open System.IO

let parseData file =
    File.ReadAllLines file
    |> (fun s -> s.[0].Split ',')
    |> Array.map int
    |> List.ofArray

let rec dayGoesBy (fish : (int*int64) list) =
    match fish with
    | []             -> []
    | (0,   n)::fish -> (6, n) :: (8, n) :: dayGoesBy fish
    | (timer, n)::fish -> (timer - 1, n) :: dayGoesBy fish

let rec simulate (days : int) (fish : (int*int64) list) : (int*int64) list =
    if days = 0
    then fish
    else dayGoesBy fish |> simulate (days - 1)

let lanternfish =
    parseData "6.txt"
    |> List.countBy id
    |> List.sortBy (fun (timer,_) -> timer)
    |> List.map (fun (timer, n) -> (timer, int64 n))

prn lanternfish

let _80DaysOfLanternfish =
    simulate 80 lanternfish
    |> List.sumBy (fun (_,count) -> count)

prn _80DaysOfLanternfish

(* Part two - 256 days of lanternfish *)
let rec mergeEntries acc l =
    match l with
    | []  -> acc
    | [f] -> acc @ [f]
    | (t1, n1)::(t2, n2)::ls ->
        if t1 = t2
        then mergeEntries ((t1, n1 + n2)::acc) ls
        else mergeEntries ((t1,n1)::acc) ((t2,n2)::ls)

let rec simulate' (days : int) (fish : (int*int64) list) : (int*int64) list =
    if days = 0
    then fish
    else
        dayGoesBy fish
        |> List.sortBy (fun (timer,_) -> timer)
        |> mergeEntries []
        |> simulate' (days - 1)

let _256DaysOfLanternfish = simulate' 256 lanternfish
List.sumBy (fun (_,count) -> count) _256DaysOfLanternfish |> prn

