namespace DepParsing.Graph

open System

/// Graph-based first-order dependency parser based on Eisner's algorithm
type Parser(create: Tree -> Tree -> bool -> Tree) =
    let chart: Tree[][] =
        let count = 512
        Array.init count (fun _ -> Array.zeroCreate count)

    let merge (left: Tree) (right: Tree) lmost i j =
        let t = create left right lmost
        if t <> null then
            let o = chart.[i].[j]
            if o = null || o.Score < t.Score then chart.[i].[j] <- t

    let reset length =
        for i = 0 to length - 1 do
            Array.Clear(chart.[i], 0, length)

    /// First-order Eisner algorithm
    member this.Decode(data: 'T[]) =
        reset data.Length
        for i = 0 to data.Length - 1 do chart.[i].[i] <- Tree(data.[i])

        for j = 0 to data.Length - 1 do
            for i = j - 1 downto 0 do

                // (lmost, rmost)
                for k = i to j - 1 do
                    let left, right = chart.[i].[k], chart.[j].[k + 1]
                    if left <> null && right <> null then
                        merge left right true i j
                        if i > 0 then merge left right false j i // excludes ROOT

                // (lmost, lmost) and (rmost, rmost)
                for k = i + 1 to j - 1 do
                    let left, right = chart.[i].[k], chart.[k].[j]
                    if left <> null && right <> null then
                        if left.Right.Rmost then merge left right true i j // excludes complete span

                    let left, right = chart.[k].[i], chart.[j].[k]
                    if left <> null && right <> null then
                        if right.Left.Lmost then merge left right false j i // excludes complete span

        let top = chart.[0].[data.Length - 1]
        top