namespace DepParsing

module Eval =
    
    open System
    open System.Collections.Generic

    let run (data: (Token * int)[]) =
        let excl = [| "''"; ","; "."; ":"; "``" |] |> HashSet // punctuation is usually excluded for evaluation
        
        let mutable uas, las, count = 0, 0, 0
        for t, h in data do
            if Token.FormDict.[t.Form] |> excl.Contains |> not then
                if t.Head = h then uas <- uas + 1
                count <- count + 1
        Console.WriteLine("UAS:\t{0:0.00000}", single uas / single count)