open System

let data  = IO.File.ReadAllLines "input"
let prn msg value = printfn $"{msg}: {value}"

(* Day 5, Part 1 - Hoorrrayyy! :D *)
let parseData (data : string array) =
    let isRule : (string -> bool) = _.Contains("|")
    let isUpdate : (string -> bool) = _.Contains(",")
    data
    |> Array.fold (fun (rules, updates) s -> 
            if isRule s
            then 
                let newRules =
                    let left = s[..1] |> int16
                    let right = s[3..] |> int16
                    rules
                    |> Map.add left (
                        match Map.tryFind left rules with
                        | None -> Set.empty.Add right
                        | Some rs -> rs.Add right
                    )
                (newRules, updates)
            else
            if isUpdate s
            then
                let update = s.Split([|','|]) |> Array.map int16 |> List.ofArray
                (rules, update :: updates)
            else rules, updates
        ) (Map.empty, [])

let rules, updates = parseData data

let isCorrectOrder (rules : Map<int16,Set<int16>>) update =
    let rec check = function
        | [] -> true
        | p :: pages -> if pages |> List.forall rules[p].Contains then check pages else false

    check update

let getMiddlePageNumber (update : int16 list) = update[update.Length / 2]

updates
|> List.filter (isCorrectOrder rules)
|> List.map getMiddlePageNumber
|> List.sum
|> prn "Day 5, part 1! MERRY CHRISTMAS, PEEPS:) "


(* Day 5, Part 2 - Yay! Christmas! *)

let fixOrder (rules : Map<int16,Set<int16>>) update =
    let rec build = function
        | [] -> []
        | p :: pages ->
            let rightOrder, wrongOrder = pages |> List.partition rules[p].Contains
            if wrongOrder.IsEmpty
            then p :: build rightOrder 
            else build (wrongOrder @ [p] @ rightOrder)

    build update

updates
|> List.filter (isCorrectOrder rules >> not)
|> List.map (fixOrder rules)
|> List.map getMiddlePageNumber
|> List.sum
|> prn "Day 5, part 2"