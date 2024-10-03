#r "./libs/FParsec.dll"
#r "./libs/FParsecCS.dll"
#r "nuget: FSharp.Data"

type Trade = {
    MakerAccountId: string
    TakerAccountId: string
    Symbol: string
    Side: Side
    Price: int64
    Quantity: uint32
} and
    Side =
        | Bid
        | Ask

type Market = {
    vwap: float
    volume: uint32
}

open FParsec

let pTakerSide : Parser<Side, unit> = (pstringCI "Bid" >>% Bid) <|> (pstringCI "Ask" >>% Ask)
