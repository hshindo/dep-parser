namespace DepParsing.Traisition

[<AllowNullLiteral>]
type IState =
    abstract member IsFinal: bool
    abstract member Next: int -> IState
    abstract member NextActs: unit -> int[]
    abstract member GetFeats: int -> int[]