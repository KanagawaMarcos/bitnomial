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
let pCsvContent = manyChars (noneOf [','; '\n'; '\r'])
let pCsvValue : Parser<string,unit> = pCsvContent .>> (pchar ',')
let pTakerSide : Parser<Side, unit> = (pstringCI "Bid" >>% Bid) <|> (pstringCI "Ask" >>% Ask) .>> pchar ','
let pPrice = pint64 .>> pchar ','
let pQuantity = puint32 .>> optional (pchar ',')
let pCsvLine =
        pCsvValue .>>. pCsvValue .>>. pCsvValue .>>. pTakerSide .>>. pPrice .>>. pQuantity
        |>> (fun (((((maker, taker), symbol), side), price), qty) ->
            { MakerAccountId = maker
              TakerAccountId = taker
              Symbol = symbol
              Side = side
              Price = price
              Quantity = qty })

 
