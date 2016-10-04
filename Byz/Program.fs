// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open FSharp.Control

type TopDownMessage(id, messVal, subArraySize) = 
    //let potato = 1

    let childMessages = Array.create subArraySize (TopDownMessage("blank", "-1", subArraySize - 1))

    let messageSent = false

    //[<DefaultValue>]
    //val mutable private ChildMessages:Message

    member x.AttachMessage (incmess : TopDownMessage) = 
        let firstidwidth = incmess.ID.IndexOf(".")
        if firstidwidth > -1 then
            let subid = Int32.Parse (incmess.ID.Substring(0, firstidwidth))
            childMessages.[subid].AttachMessage (TopDownMessage(incmess.ID.Substring(subid + 1), messVal, subArraySize - 1))
        else
            let subid = Int32.Parse incmess.ID
            childMessages.[subid] <- TopDownMessage(incmess.ID, messVal, subArraySize - 1)

    member x.ID : String = 
        id

type General(id, initVal, isFaulty, numGenerals, ?someMessages) = 
    let messages = 
        match someMessages with
        | Some x -> x
        | None -> Array.empty<String>

    let lambda = TopDownMessage("lambda", initVal, numGenerals)

    member x.IsFaulty =
        isFaulty = "1"

    member x.ID = 
        id

(*type Message =
    | TDMess of TopDownMessage
    | SMess of StepMessage
    | BUMess of BottomUpMessage*)


let mailboxGeneral (inbox : MailboxProcessor<TopDownMessage>, thisGeneral : General) = 
    let general = General("8", "9", "1", 6)
    let rec loop count = 
        async { let! msg = inbox.Receive()
                printfn "Message is %A" msg.ID
                return! loop 0}
    loop 0

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    let reader = File.OpenText(argv.[0])
    let mutable fileLine = reader.ReadLine()
    let mutable linePieces = fileLine.Split()
    let numNodes = Int32.Parse linePieces.[0]
    let v0 = linePieces.[1]
    let genArray = Array.create numNodes (General("8", "9", "1", 6))
    for i = 0 to numNodes - 1 do
        fileLine <- reader.ReadLine()
        linePieces <- fileLine.Split()
        if linePieces.[2] = "1" then
            genArray.[i] <- General(linePieces.[0], linePieces.[1], linePieces.[2], numNodes, linePieces.[3..])
        else
            genArray.[i] <- General(linePieces.[0], linePieces.[1], linePieces.[2], numNodes)

    Array.sortInPlaceBy (fun (x : General) -> x.ID) genArray

    //let mbg = new MailboxProcessor<TopDownMessage>(mailboxGeneral)
    (*let general = new MailboxProcessor<TopDownMessage>(fun inbox ->
        let rec loop count =
            async { let! msg = inbox.Receive()
                    printfn "Message is %A" msg.ID
                    return! loop(count + 1)}
        loop 0)*)



    0 // return an integer exit code
