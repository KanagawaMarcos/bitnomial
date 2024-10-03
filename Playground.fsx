#r "./libs/FParsec.dll"
#r "./libs/FParsecCS.dll"
#r "nuget: FSharp.Data"
open System
open FSharp.Data
open FParsec

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
let parse log =
    match run pLedger log with
    | Success(result, _, _) -> result
    | Failure(errorMsg, _, _) -> failwith errorMsg

let calculate trades =
    trades
    |> Seq.groupBy (fun trade -> trade.Symbol)
    |> Seq.map (fun (symbol, trades) ->
        let totalVolume = trades |> Seq.sumBy (fun t -> t.Quantity)
        let totalQuantityPrice = trades |> Seq.sumBy (fun t -> float t.Quantity * float t.Price)
        let vwap = totalQuantityPrice / float totalVolume
        symbol, { vwap = vwap; volume = totalVolume }
    )
    |> Map.ofSeq
    
let output data =
    let json = 
        data
        |> Map.toSeq
        |> Seq.map (fun (symbol, report) ->
            symbol, 
            JsonValue.Record [|
                "vwap", JsonValue.Float report.vwap
                "volume", JsonValue.Number (decimal report.volume)
            |])
        |> Seq.toArray
        |> JsonValue.Record
    json.ToString()


let csv = """Tyrell Corp A123,Wayland-Yutani Corp BC32,BUSU1,Bid,42,10
CHOAM Arakis Z23,OPEC 897,BUIZ1,Ask,-2,14
InGen Tech BCZ232,BioSynFG332,BUSM2,Bid,43250,23"""

let trades = parse csv
let stdout = output (calculate trades)
Console.WriteLine(stdout)