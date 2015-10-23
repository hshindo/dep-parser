namespace DepParsing

open System

type Token(id: int, form: int, cat: int, head: int) =
    member this.Id = id
    member this.Form = form
    member this.Cat = cat
    member this.Head = head

    override this.ToString() = String.Format("{0} {1} {2} {3}", id, form, cat, head)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Token =

    let digitToZero (input: string) = Text.RegularExpressions.Regex.Replace(input, @"\d", "0")

    let FormDict = IdDictionary()
    let CatDict = IdDictionary()

    let inline split (pred: _ -> bool) (source: seq<_>) =
        seq {
            let buf = ResizeArray()
            for item in source do
                if pred item && buf.Count > 0 then
                    yield buf.ToArray()
                    buf.Clear()
                else buf.Add item
            }

    let read path =
        let conv (text: string) =
            let items = text.Split '\t'
            let id = items.[0] |> int
            let form = items.[1].ToLower() |> digitToZero
            let form = FormDict.Add(form)
            //let form = form |> (fun s -> FormDict.Add s)
            let cat = items.[4] |> CatDict.Add
            let head = items.[6] |> int
            Token(id, form, cat, head)
        let f (lines: string[]) =
            let root = Token(0, -1, -1, -1)
            lines |> Array.map conv |> Array.insert 0 root
        IO.File.ReadLines path
        |> split (fun s -> s.Length = 0)
        |> Seq.map f
        |> Seq.toArray