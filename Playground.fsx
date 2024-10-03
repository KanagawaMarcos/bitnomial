#r "./libs/FParsec.dll"
#r "./libs/FParsecCS.dll"
#r "nuget: FSharp.Data"


open System

let input = Console.In.ReadToEnd()

type Side = Bid | Ask

type Trade = {
    MakerAccountId: string
    TakerAccountId: string
    Symbol: string
    Side: Side
    Price: int64
    Quantity: uint32
}

type ProductReport = {
    vwap: float
    volume: uint32
}

open FParsec
