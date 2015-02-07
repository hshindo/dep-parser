namespace TransitionParsing

type Node(act: int, state: IState, feats: int[], weight: float) =
    member this.Act = act
    member this.Feats = feats
    member this.Weight = weight
    member this.State = state

//    public Forward: IEnumerable[Node]
//            get
//                when (Prev != null)
//                    foreach (n in Prev.Forward) yield n
//                yield this
//                
//        public Backward: IEnumerable[Node]
//            get
//                yield this
//                when (Prev != null)
//                    foreach (n in Prev.Backward) yield n

type Decoder() =
    let beamSize = 3
    let weights: float[] = Array.zeroCreate (1 <<< 20)

    let expand source =
        let temp = ResizeArray(beamSize)
        for node: Node in source do
            for act in node.State.NextActs() do
                let fs = node.State.GetFeats act
                let w = fs |> Array.sumBy (fun id -> weights.[id])
                (w, node, act, fs) |> temp.Add
        temp.Sort(fun (w1, _, _, _) (w2, _, _, _) -> w1.CompareTo w2)
            
        let dest = ResizeArray(beamSize)
        let mutable count = min beamSize temp.Count
        for i = 0 to count do
            let w, node, act, fs = temp.[i]
            let st = node.State.Next(act)
            Node(act, st, fs, w) |> dest.Add
            
        dest

    member this.Weights = weights

    member this.Search(input: IState) =
        let mutable kbest = ResizeArray()
        Node(-1, input, [||], 0.0) |> kbest.Add
        while kbest.Count > 0 && not kbest.[0].State.IsFinal do
            kbest <- expand kbest
        kbest.[0]