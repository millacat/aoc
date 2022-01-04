open System.IO
open IOz
open System.Text.RegularExpressions

type Board = (int * bool) List List

let (|ParseRegex|_|) regex str =
   let m = Regex.Match(str, regex)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue)
   then Some intvalue
   else None

let (|Line|_|) (s : string) =
    match s with
    | ParseRegex "(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)"
                 [Integer d1; Integer d2; Integer d3; Integer d4; Integer d5]
        -> Some [d1;d2;d3;d4;d5]
    | _ -> None

let getLine = function
    | Line nums -> nums
    | _ -> failwith "Bad input data"

let makeBoard (l : string List) : int List List = List.map getLine l

let rec getBoards (data : string List) : int List List List =
    match data with
    | [] -> []
    | ""::rest -> getBoards rest
    | all -> makeBoard all.[..4] :: getBoards all.[5..]

let parseData (file: string)  =
    let lines = File.ReadAllLines file
    let first : string = lines.[0]
    let rest : string List = lines.[1..] |> List.ofArray
    let rndOrder = first.Split ',' |> List.ofArray |> List.map int
    let boards = getBoards rest
                 |>
                 List.map (fun board ->
                             List.map (fun row ->
                                 List.map (fun n ->
                                     (n,false))
                                  row) board)
    (rndOrder, boards)

(* a board is a list of lists of tuples of integers and bools, where the bool
 * represents if the integer has been marked *)
let (rndOrder, boards) = parseData "4.txt"

let markBoards (drawn : int) (boards : Board List) : Board List =
    List.map (fun board ->
        List.map (fun row ->
            List.map (fun (num,b) -> if num = drawn
                                     then (num,true)
                                     else (num,b))
             row) board) boards

let checkBoard (board : Board) : bool =
    let checkRows board' =
        List.map (fun row -> List.fold (fun acc (_,b) -> acc && b) true row) board'
        |> List.contains true
    let row = checkRows board
    let col = List.transpose board |> checkRows
    row || col

//let b = boards.Head
//let m = markBoards 83 [b] |> markBoards 11 |> markBoards 47 |> markBoards 61 |> markBoards 45
//                          |> markBoards 30 |> markBoards 53 |> markBoards 64 |> markBoards 26
//prn m.Head
//prn <| checkBoard m.Head

let sumBoard board =
    List.map (fun row ->
                 List.fold (fun acc (n,b) ->  if b then acc else acc + n) 0 row
             ) board
    |> List.sum

let rec checkBoards boards n =
    match boards with
    | [] -> 0
    | b::bs -> if checkBoard b then sumBoard b else checkBoards bs n


let rec playBingo boards rndOrder =
    match rndOrder with
    | [] -> failwith "No One Got The Bingos! {o_O}"
    | n::ns ->
        let markedBoards = markBoards n boards
        let bingo = checkBoards markedBoards n
        in if bingo = 0
           then playBingo markedBoards ns
           else bingo * n

playBingo boards rndOrder |> prn

(* Part Two *)
(* Let squiddy win - What is the score of the last board to win? *)
let rec playLosingBingo rndOrder boards =
    match rndOrder with
    | [] -> failwith "No One Got The Bingos! {O.o}"
    | n::ns ->
        match markBoards n boards with
        | []  -> failwith "No one bingoed"
        | [b] -> if checkBoard b // Only one board left! is it a winner?
                 then sumBoard b |> (*) n
                 else playLosingBingo ns [b]
        | bs ->
            List.filter (checkBoard >> not) bs
            |>
            playLosingBingo ns

playLosingBingo rndOrder boards |> prn

