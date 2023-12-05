module IOz
open System.IO

(* Read data from file into a list *)
let getData (file : string) (convert : string -> 'a): 'a List =
    let s = new StreamReader(file)
    let rec fetch (xs : 'a list) : 'a List =
        match s.ReadLine() with
        | null -> xs
        | line -> convert line :: fetch xs
    fetch []

(* print anything {^.0} *)
let prn (inp : 'a) : unit = printfn "%A" inp

