open IOz

(* MERRY CHRISTMAZ *)

(* Part One *)
let instructions : string List = getData "2.txt" id

type Position  = int * int
type Direction = Forward of int | Down of int | Up of int

let (|CheckDirection|_|) (name : string) (instruction : string) : Option<string> =
    if instruction.StartsWith name
    then Some <| instruction.Substring name.Length
    else None

let stringToDirection (str : string) : Direction =
    match str with
    | CheckDirection "up "      n -> Up      <| int n
    | CheckDirection "forward " n -> Forward <| int n
    | CheckDirection "down "    n -> Down    <| int n
    | _ -> failwith "ehhh.. not a direction, buddy. {O.o}"

let rec move ((v,h) : Position) (instruction : Direction) : Position =
    match instruction with
    | Up      n -> (v-n, h)
    | Forward n -> (v, h+n)
    | Down    n -> (v+n, h)

let mult ((x,y) : Position) : int = x*y

// Calculate result
let directions = List.map stringToDirection instructions
List.fold move (0,0) directions
    |> mult
    |> prn


(* Part Two *)
let rec aimAndMove ((v,h,aim) : int*int*int)
                   (instruction : Direction) : int*int*int =
    match instruction with
    | Up      n -> (v,       h,   aim-n)
    | Forward n -> (v+aim*n, h+n, aim  )
    | Down    n -> (v,       h,   aim+n)

let (v,h,_) = List.fold aimAndMove (0,0,0) directions
mult (v,h)
    |> prn

