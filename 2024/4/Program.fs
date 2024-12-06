open System

let data  = IO.File.ReadAllLines "input"
let prn msg value = printfn $"{msg}: {value}"

(* Day 4, Part 1 - XMAS SAMX!!! *)
let words = ["XMAS"; "SAMX" ]
let xmasLen = "XMAS".Length

let getDiagonals x y xs wordLen (data : string array) : string list option =
        if y + wordLen > data[0].Length || x + wordLen > data.Length
        then None
        else let ys1 =  [0 .. wordLen - 1] |> List.map ((+) y)
             let ys2 = ys1 |> List.rev
             Some [
                 (xs, ys1) ||> List.fold2 (fun acc x y -> acc + string (data[x][y])) ""
               ; (xs, ys2) ||> List.fold2 (fun acc x y -> acc + string (data[x][y])) ""
             ]

let matchesWord (words : string list) (data : string array) x y =
    let horizontal = if y + xmasLen > data[0].Length then None else Some [ data[x][y .. y + xmasLen - 1] ]
    let xs = [x .. x + xmasLen - 1]
    let vertical = if x + xmasLen > data.Length then None else Some [ xs |> List.fold (fun acc x -> acc + string (data[x][y])) "" ]
    let diagonals = getDiagonals x y xs xmasLen data
    let extractions = Option.defaultValue [] horizontal @ Option.defaultValue [] vertical @ Option.defaultValue [] diagonals

    words 
    |> List.map (fun word -> extractions |> List.filter ((=) word) |> _.Length)
    |> List.sum


let count words (data : string array) matcher =
    let ys = [0 .. data[0].Length - 1]
    let xs = [0 .. data.Length - 1]
    xs 
    |> List.map (fun x -> 
            ys
            |> List.map (matcher words data x)
            |> List.sum)
    |> List.sum

count words data matchesWord
|> prn "Day 4, part 1 here we go XMAS/SAMX"

(* Day 4, part 2 - All I want for x-mas *)

let matchMasses words data x y  =
    let xs = [x .. x + "MAS".Length - 1]
    let maybeDiagonals = getDiagonals x y xs  "MAS".Length data

    maybeDiagonals 
    |> Option.map (fun diagonals -> if diagonals |> List.forall (fun diagonal -> words |> List.contains diagonal) then 1 else 0)
    |> Option.defaultValue 0

let masses = ["MAS"; "SAM"]
let countMasses = count masses data matchMasses

countMasses
|> prn "Day 4, part 2 X-MAS result"



(* Test part 1 *)

let test = [| 
    "mXMADsXMASmsms"
    ".MEsJAM1..S..t"
    "BAaJHU..1A.A.."
    ".9JD.Y..M1..M."
    "xJ..F..X..1..X"
|]

count ["xmas"; "samx"] test matchesWord
|> prn "Number of xmas/samx"
