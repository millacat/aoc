open IOz

(*** 1st December ***)

(* Part One *)
let depths : int List = getData "1.txt" int

let rec countIncreases (depths : int List) : int =
    match depths with
    | d0::d1::ds -> let increase = if d0 < d1 then 1 else 0
                    increase + countIncreases (d1::ds)
    | _ -> 0

let rec countIncreases' (depths : int List) (acc : int) : int =
    match depths with
    | d0::d1::ds -> let increase = if d0 < d1 then 1 else 0 in
                    countIncreases' (d1::ds) (acc + increase)
    | _ -> acc

prn <| countIncreases depths
prn <| countIncreases' depths 0

(* Part Two *)
(* Sliding windows of size 3: Compare sums of windows *)
let rec countIncreasesWindow (depths : int List) (acc : int) : int =
    match depths with
    | d0::d1::d2::d3::ds ->
        let windowSum  = d0 + d1 + d2
        let windowSum' = d1 + d2 + d3
        let increase   = if windowSum < windowSum' then 1 else 0 in
        countIncreasesWindow (d1::d2::d3::ds) (acc + increase)
    | _ -> acc

prn <| countIncreasesWindow depths 0

