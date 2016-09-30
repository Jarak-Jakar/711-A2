// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open FSharp.Control

type Message(id, messVal, subArraySize) = 
    //let potato = 1

    let childMessages = Array.create subArraySize (Message("blank", "-1", subArraySize - 1))

    let messageSent = false

    (*[<DefaultValue>]
    val mutable private ChildMessages:Message*)

    member x.AttachMessage (incmess : Message) = 
        let firstidwidth = incmess.ID.IndexOf(".")
        if firstidwidth > -1 then
            let subid = Int32.Parse (incmess.ID.Substring(0, firstidwidth))
            childMessages.[subid].AttachMessage (Message(incmess.ID.Substring(subid + 1), messVal, subArraySize - 1))
        else
            let subid = Int32.Parse incmess.ID
            childMessages.[subid] <- Message(incmess.ID, messVal, subArraySize - 1)

    member x.ID : String = 
        id

type General(id, initVal, isFaulty, numGenerals, ?someMessages) = 
    let messages = 
        match someMessages with
        | Some x -> x
        | None -> Array.empty<String>

    let lambda = Message("lambda", initVal, numGenerals)

    member x.IsFaulty =
        isFaulty = "1"

    member x.ID = 
        id

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    let reader = File.OpenText(argv.[0])
    let mutable fileLine = reader.ReadLine()
    let mutable linePieces = fileLine.Split()
    let numNodes = Int32.Parse linePieces.[0]
    let v0 = linePieces.[1]
    let general = new MailboxProcessor<Message>(fun inbox ->
        let rec loop count =
            async { let! msg = inbox.Receive()
                    printfn "Message is %A" msg.ID
                    return! loop(count + 1)}
        loop 0)
    0 // return an integer exit code
