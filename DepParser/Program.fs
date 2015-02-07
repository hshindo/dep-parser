open System

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv

    let path = @""
    let traindata = Token.read (path + "wsj_02-21.conll")
    let t = Trainer()
    t.Train traindata

    0 // return an integer exit code
