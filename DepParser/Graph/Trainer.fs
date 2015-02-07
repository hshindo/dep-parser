namespace DepParsing.Graph

open DepParsing
open System

type Trainer() =
    let weights = Array.zeroCreate (1 <<< 23)

    let showProgress (value: int) (maximum: int) =
        assert (value >= 0)
        if value >= maximum then
            Console.CursorLeft <- 0
            String('#', 20) |> Console.Write 
            Console.WriteLine ""
        else
            let p = value * 20 / maximum
            Console.CursorLeft <- 0
            String('#', p) |> Console.Write
            String('-', 20 - p) |> Console.Write

    let toFeats (t1: Tree) (t2: Tree) (lmost: bool) =
        let toHash array = array |> (fun x -> abs (Array.toHash x) % weights.Length)
        let form = if lmost then -1 else 1
        let tx, ty = t1.Value, t2.Value
        [|
            [| "f-f".GetHashCode(); tx.Form; ty.Form; form |] |> toHash
            [| "t-t".GetHashCode(); tx.Cat; ty.Cat; form |] |> toHash
            [| "f-t".GetHashCode(); tx.Form; ty.Cat; form |] |> toHash
            [| "t-f".GetHashCode(); tx.Cat; ty.Form; form |] |> toHash
        |]

    let psr = Parser(fun t1 t2 lmost ->
        let feats = toFeats t1 t2 lmost
        let score = feats |> Array.sumBy (fun x -> weights.[x])
        let score = score + t1.Score + t2.Score
        Tree(t1, t2, lmost, feats, score)
        )

    let goldPsr = Parser(fun t1 t2 lmost ->
        let v1, v2 = t1.Value, t2.Value
        let b = (v1.Id = v2.Head && lmost) || (v1.Head = v2.Id && not lmost)
        if b then
            let feats = toFeats t1 t2 lmost
            let score = feats |> Array.sumBy (fun x -> weights.[x])
            let score = score + t1.Score + t2.Score
            Tree(t1, t2, lmost, feats, score)
        else null
        )

    member this.Train(traindata: Token[][]) =
        let traindata = traindata |> Array.take 5000

        let mutable count = 0
        for iter = 1 to 5 do
            ("iter {0}:", iter) |> Console.WriteLine
            let data = traindata
            Array.shuffle data
            let infs = ResizeArray()
            for i = 0 to data.Length - 1 do
                //if i % 100 = 0 then i |> Console.WriteLine
                if i % 100 = 0 || i + 1 = data.Length then showProgress (i + 1) data.Length
                let sent = data.[i]

                let y = goldPsr.Decode sent
                let z = psr.Decode sent
                for n in y.Topdown do
                    if not n.IsLeaf then
                        n.Feats |> Array.iter (fun f -> weights.[f] <- weights.[f] + 1.0)
                for n in z.Topdown do
                    if not n.IsLeaf then
                        n.Feats |> Array.iter (fun f -> weights.[f] <- weights.[f] - 1.0)

                let hy = y.ToHeads sent.Length
                let hz = z.ToHeads sent.Length
                Array.zip sent.[1 .. sent.Length - 1] hz |> infs.Add

            infs.ToArray() |> Array.concat |> Eval.run
        ()
