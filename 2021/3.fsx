open IOz

(* It's Christmas, you know *)

(* Part 1. Transpose! *)
let stringToCharList (s : string) : char List = [for c in s do yield c]

let aListToString l =
    let l' = List.rev l
    let b = List.fold (fun acc x -> (string x) + acc) "" l'
    in "0b" + b

let mult x y = (int x) * (int y)

let data : char List List = getData "3.txt" stringToCharList
let transposed = List.transpose data

let counts' = List.map (fun originalColumn ->
                 List.fold (fun (z,o) n -> if n = '1'
                                           then (z,o+1)
                                           else (z+1,o))
                            (0,0) originalColumn
                       ) transposed

let gamma' = List.map (fun (z,o) -> if z > o then 0 else 1) counts'
                |> aListToString

let epsilon' = List.map (fun (z,o) -> if z > o then 1 else 0) counts'
                |> aListToString

let res' = mult gamma' epsilon'
prn <| res'

(* Part 1 again. No transpose {*.*}  *)

let rec initList (n : int) : (int*int) List =
    if n > 0
    then (0,0) :: initList (n-1)
    else []

let traverse (data : char List List) : (int*int) List =

    let rec count (binary : char List) (zeroOne : (int*int) List)
                 : (int*int) List =
        match binary, zeroOne with
        | '0'::rs, (z,o)::zo -> (1+z, o) :: count rs zo
        | '1'::rs, (z,o)::zo -> (z, 1+o) :: count rs zo
        | _, _ -> []

    let rec start data zeroOnes : (int*int) List =
        match data with
        | [] -> zeroOnes
        | str::rest -> let newZeroOnes = count str zeroOnes
                       in start rest newZeroOnes

    let len = data.[0].Length
    start data (initList len)

let counts : (int*int) List = traverse data

// The most common bits
let gamma = List.map (fun (z,o) -> if z > o then 0 else 1) counts
                |> aListToString

// The least common bits
let epsilon = List.map (fun (z,o) -> if z > o then 1 else 0) counts
                |> aListToString

let res = mult gamma epsilon
prn res

(* Part Two *)
let rec filtering (counts : (int*int) List) (idx : int) (data : string List)
    (pred : int -> int -> bool) : string List =
    let countsIdx = counts.[idx..]
    match countsIdx with
    | (z,o)::_ ->
        let newData = if pred z o
                      then List.filter (fun (s: string) -> s.[idx] = '0') data
                      else List.filter (fun (s: string) -> s.[idx] = '1') data
        if newData.Length = 1
        then newData
        else
            let newCounts = traverse <| List.map stringToCharList newData
            filtering newCounts (idx+1) newData pred
    | _ -> []

let data' : string List = getData "3.txt" id
let predOxy z o = z > o
let predCO2 z o = o >= z

let oxyGenRating = "0b" + (filtering counts 0 data' predOxy).Head
let CO2Rating = "0b" + (filtering counts 0 data' predCO2).Head

let res'' = mult oxyGenRating CO2Rating
prn res''

