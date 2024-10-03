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
    
open FParsec
let pCsvContent : Parser<string,unit> = manyChars (noneOf [','; '\n'; '\r'])
let pCsvValue : Parser<string,unit> = pCsvContent .>> optional (pchar ',')
let pTakerSide : Parser<Side, unit> = (pstringCI "Bid" >>% Bid) <|> (pstringCI "Ask" >>% Ask) .>> pchar ','
let pPrice : Parser<int64, unit> = pint64 .>> pchar ','
let pQuantity : Parser<uint32, unit> = puint32 .>> optional (pchar '\n' <|> pchar '\r')
let pCsvLine =
        pCsvValue .>>. pCsvValue .>>. pCsvValue .>>. pTakerSide .>>. pPrice .>>. pQuantity
        |>> (fun (((((maker, taker), symbol), side), price), qty) ->
            { MakerAccountId = maker
              TakerAccountId = taker
              Symbol = symbol
              Side = side
              Price = price
              Quantity = qty })

 
let pLedger = many pCsvLine
let parse trades =
    match run pLedger trades with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg
    
let csv = """
Tyrell Corp A123,Wayland-Yutani Corp BC32,BUSU1,Bid,42,10
CHOAM Arakis Z23,OPEC 897,BUIZ1,Ask,-2,14
InGen Tech BCZ232,BioSynFG332,BUSM2,Bid,43250,23
"""

let parsedTrades = parse csv

parsedTrades |> List.iter (fun trade ->
    printfn $"MakerAccountId: %s{trade.MakerAccountId}, TakerAccountId: %s{trade.TakerAccountId}, Symbol: %s{trade.Symbol}, Side: %A{trade.Side}, Price: %d{trade.Price}, Quantity: %d{trade.Quantity}")