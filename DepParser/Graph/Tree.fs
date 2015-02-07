namespace DepParsing.Graph

open DepParsing
open System

[<AllowNullLiteral>]
type Tree private(value: Token, left: Tree, right: Tree, lmost: bool, feats: int[], score: float) =
    
    new(value) = Tree(value, null, null, true, [||], 0.0)
    new(left: Tree, right: Tree, lmost, feats, score) =
        let value = if lmost then left.Value else right.Value
        Tree(value, left, right, lmost, feats, score)

    member this.Value = value
    member this.Left = left
    member this.Right = right
    member this.Lmost = lmost || this.IsLeaf
    member this.Rmost = not lmost || this.IsLeaf
    member this.Feats = feats
    member this.Score = score

    member this.IsLeaf = left = null && right = null
    member this.Topdown =
        seq {
            yield this
            if this.Left <> null then yield! this.Left.Topdown
            if this.Right <> null then yield! this.Right.Topdown
            }

    member this.ToHeads(length: int) =
        //if node.I <> 0 then failwith ""
        let heads = Array.zeroCreate length
        for n in this.Topdown do
            let id = n.Value.Id
            if not n.IsLeaf then
                if n.Left.Value.Id <> id then heads.[n.Left.Value.Id] <- id
                if n.Right.Value.Id <> id then heads.[n.Right.Value.Id] <- id
        heads |> Array.skip 1

    override this.ToString() = String.Format("{0} {1}", value, lmost)
