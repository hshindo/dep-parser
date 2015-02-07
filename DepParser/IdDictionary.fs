namespace DepParsing

open System
open System.Collections.Generic
open System.IO

type IdDictionary<'T> (comparer: IEqualityComparer<'T>) =
    let keyToId: Dictionary<'T, int> = Dictionary(comparer)
    let idToKey: List<'T> = List()
    let idToCount: List<int> = List()
    let openIds: Queue<int> = Queue()

    member val Total = 0 with get, set // how to do private set ?

    new () = IdDictionary EqualityComparer<'T>.Default

    member this.Count = keyToId.Count
    member this.Ids =
        seq {
            for i in 0 .. this.Size - 1 do
                if this.ContainsId i then yield i
            }
    member this.Keys = this.Ids |> Seq.map (fun id -> this.[id])
    member this.Item with get (id: int) = idToKey.[id]
    member this.Size = idToKey.Count

    member this.Add (key: 'T) = this.Add(key, 1)
    member this.Add (key: 'T, count: int) =
        assert (count >= 1)
        let mutable b, id = keyToId.TryGetValue key
        if b = false then
            if openIds.Count > 0 then
                id <- openIds.Dequeue ()
                idToKey.[id] <- key
            else
                id <- this.Count
                idToKey.Add key
            keyToId.Add (key, id)
        if id = idToCount.Count then idToCount.Add count
        else idToCount.[id] <- idToCount.[id] + count
        this.Total <- this.Total + count
        id

    member this.ContainsId (id: int) = id >= 0 && id < this.Size && this.CountOf id > 0
    member this.ContainsKey (key: 'T) = keyToId.ContainsKey(key)
    member this.CountOf (id: int) = idToCount.[id]
    member this.Delete (id: int) = this.Remove(id, (this.CountOf id))

    member this.Read (path: string, decode: string -> 'T) =
        File.ReadLines path |> Seq.iteri (fun i line ->
            if line.Length = 0 then openIds.Enqueue i
            else
                let items = line.Split '\t'
                let key = decode items.[0]
                keyToId.Add (key, i)
                idToKey.Add key
                items.[1] |> Int32.Parse |> idToCount.Add )

    member this.Remove (id: int) = this.Remove(id, 1)
    member this.Remove (id: int, count: int) =
        assert (count <= this.CountOf id)
        idToCount.[id] <- idToCount.[id] - count
        this.Total <- this.Total - count
        if idToCount.[id] = 0 then
            keyToId.Remove this.[id] |> ignore
            openIds.Enqueue id

    member this.Trim () =
        let mutable i = this.Size
        while this.ContainsId i = false do i <- i - 1
        idToKey.RemoveRange (i + 1, idToKey.Count - i - 1)
        idToCount.RemoveRange (i + 1, idToCount.Count - i - 1)
        assert (idToKey.Capacity = idToCount.Capacity)

    member this.ToId (key: 'T) = this.ToId (key, -1)
    member this.ToId (key: 'T, def: int) = if this.ContainsKey key then keyToId.[key] else def

    member this.Write (path: string) =
        this.Trim ()
        let lines = Array.init this.Size (fun i ->
            if this.ContainsId i then String.Format ("{0}\t{1}", this.[i], this.CountOf i)
            else "" )
        File.WriteAllLines (path, lines)