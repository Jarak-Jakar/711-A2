// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open FSharp.Control

let tcs = new System.Threading.Tasks.TaskCompletionSource<bool>()

let factorial n = 
    let rec loop i acc = 
        match i with
        | 0 | 1 -> acc
        | _ -> loop (i - 1) (acc * i)
    loop n 1

type TopDownMessage(generalID, message : string, roundNumber) = 
    //let potato = 1

    //let childMessages = Array.create subArraySize (TopDownMessage("blank", "-1", subArraySize - 1))

    let messageSent = false

    (*[<DefaultValue>]
    val mutable private ChildMessages : TopDownMessage[]

    member x.AttachMessage (incmess : TopDownMessage) = 

        if isNull x.ChildMessages && subArraySize > 0 then
            x.ChildMessages <- Array.create subArraySize (TopDownMessage("blank", "-1", subArraySize - 1))

        let firstidwidth = incmess.ID.IndexOf(".")
        if firstidwidth > -1 then
            let subid = Int32.Parse (incmess.ID.Substring(0, firstidwidth))
            x.ChildMessages.[subid].AttachMessage (TopDownMessage(incmess.ID.Substring(subid + 1), messVal, subArraySize - 1))
        else
            let subid = Int32.Parse incmess.ID
            x.ChildMessages.[subid] <- TopDownMessage(incmess.ID, messVal, subArraySize - 1)*)

    member x.ID : String = 
        generalID

    member x.Message = 
        message

    member x.RoundNumber = 
        roundNumber

type BottomUpMessage = 
    val id : int

type StepMessage(nextRound) = 
    
    member x.roundNumber = 
        nextRound

type Message =
    | TDMess of TopDownMessage
    | SMess of StepMessage
    | BUMess of BottomUpMessage

type General(id, v0, initVal, isFaulty, numGenerals, numRounds, ?someMessages) = 
    let messages = 
        match someMessages with
        (*| Some (x : String[]) -> Array2D.init (x.Length / numGenerals) numGenerals (fun y z -> x.[numGenerals * y + z])
        | None -> Array2D.zeroCreate 1 1*)
        | Some (x : String[]) -> Array.init numRounds (fun y -> String.concat " " x.[(y * numGenerals)..(y * numGenerals + numGenerals - 1)])
        | None -> Array.empty

    //let lambda = TopDownMessage("lambda", initVal, numGenerals)

    //let genArray = Array2D.init numRounds numGenerals (fun x y -> (Array.create (factorial numGenerals / factorial (numGenerals - x)) (initVal)))
    let genArray = Array.init numRounds (fun x -> (Array.create (factorial numGenerals / factorial (numGenerals - x - 1)) (initVal)))

    //let bigArray = Array2D.init numRounds numGenerals (fun x y -> (Array.create (factorial numGenerals / factorial (numGenerals - x)) 0))

    let rec compindex roundNumber numGenerals sender messageSize i genless  = 
        if roundNumber = 0 then
            sender
        else 
            let a = ((i * genless) / messageSize)
            if a < sender then
                let b = a * messageSize
                b + (compindex (roundNumber - 1) (numGenerals - 1) (sender - 1) messageSize i (genless - 1))
            else
                let b = (a + 1) * messageSize
                b + (compindex (roundNumber - 1) (numGenerals - 1) sender (messageSize / numGenerals) i (genless - 1))

    let mb = 
        MailboxProcessor.Start (fun inbox ->
            let rec loop count = 
                async { let! (msg : Message) = inbox.Receive()
                        match msg with
                        | TDMess mesg -> 
                            //printfn "Message is %A" mesg.ID
                            let sender = (Int32.Parse mesg.ID) - 1
                            let rn = mesg.RoundNumber - 1
                            let msgSize = (factorial numGenerals) / (numGenerals * (factorial (numGenerals - rn - 1)))
                            let mutable index = 0
                            //let messagePieces = mesg.Message.Split
                            for i = 0 to mesg.Message.Length - 1 do
                                //let mutable index = 0
                                (*if (i / (numGenerals - rn)) < sender then
                                    //index <- (i * msgSize * rn) + sender - rn
                                    index <- (i * msgSize) + sender - rn
                                    //index <- (i * (numGenerals - rn) * rn) + sender - rn
                                else
                                    //index <- ((i + 1) * msgSize * rn) + sender
                                    index <- ((i + 1) * msgSize) + sender
                                    //index <- ((i + 1) * (numGenerals - rn) * rn) + sender*)
                                index <- compindex rn (numGenerals - 1) sender msgSize i (numGenerals - 1)
                                let x = mesg.Message.[i]
                                if x = '0' || x = '1' then
                                    //genArray.[rn,i].[sender] <- mesg.Message.[i].ToString()
                                    genArray.[rn].[index] <- mesg.Message.[i].ToString()
                                else
                                    genArray.[rn].[index] <- v0

                            printfn "genArray = %A" genArray
                            //printfn "hi bill"
                            if count >= 21 then
                                tcs.SetResult(true)
                            //System.Threading.Thread.Sleep(5000)
                                //genArray.[rn,i].[sender] <- Int32.Parse mesg.Message[i]
                        | _ -> printfn "Matched something other than a TopDownMessage"
                        return! loop (count + 1)}
            loop 0)

    member x.Post(msg) = mb.Post(msg)

    member x.IsFaulty =
        isFaulty = "1"

    member x.ID = 
        id

(*let mailboxGeneral (inbox : MailboxProcessor<TopDownMessage>, thisGeneral : General) = 
    //let general = thisGeneral
    let rec loop count = 
        async { let! msg = inbox.Receive()
                printfn "Message is %A" msg.ID
                return! loop 0}
    loop 0*)

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    let reader = File.OpenText(argv.[0])
    let mutable fileLine = reader.ReadLine()
    let mutable linePieces = fileLine.Split()
    //let numNodes = Int32.Parse linePieces.[0]
    let numNodes = 7
    let v0 = linePieces.[1]
    let genArray = Array.create numNodes (General("8", v0, "9", "1", 4, 2))
    let numRounds = int (Math.Floor((float (numNodes - 1)) / 3.0)) + 1
    for i = 0 to numNodes - 1 do
        //fileLine <- reader.ReadLine()
        //linePieces <- fileLine.Split()
        (*if linePieces.[2] = "1" then
            genArray.[i] <- General(linePieces.[0], v0, linePieces.[1], linePieces.[2], numNodes, numRounds, linePieces.[3..])
        else
            genArray.[i] <- General(linePieces.[0], v0, linePieces.[1], linePieces.[2], numNodes, numRounds)*)
        genArray.[i] <- General(i.ToString(), v0, "0", "0", numNodes, numRounds)

    Array.sortInPlaceBy (fun (x : General) -> x.ID) genArray

//    let bill = (TDMess(TopDownMessage("1", "1", 1)))
//    genArray.[0].Post(bill)
//    let bill = (TDMess(TopDownMessage("2", "1", 1)))
//    genArray.[0].Post(bill)
//    let bill = (TDMess(TopDownMessage("3", "1", 1)))
//    genArray.[0].Post(bill)
//    let bill = (TDMess(TopDownMessage("4", "1", 1)))
//    genArray.[0].Post(bill)
//
//    let bill = (TDMess(TopDownMessage("1", "111", 2)))
//   // let ben = 
//    
//    genArray.[0].Post(bill)
//    let bill = (TDMess(TopDownMessage("2", "111", 2)))
//    genArray.[0].Post(bill)
//    let bill = (TDMess(TopDownMessage("3", "111", 2)))
//    genArray.[0].Post(bill)
//    let bill = (TDMess(TopDownMessage("4", "111", 2)))
//    genArray.[0].Post(bill)

    (*for i = 1 to numNodes do
        let bill = (TDMess(TopDownMessage(i.ToString(), "1", 1)))
        genArray.[0].Post(bill)

    for i = 1 to numNodes do
        let bill = (TDMess(TopDownMessage(i.ToString(), "111", 2)))
        genArray.[0].Post(bill)

    for i = 1 to numNodes do
        let bill = (TDMess(TopDownMessage(i.ToString(), "111111111111", 3)))
        genArray.[0].Post(bill)*)

    (*for i = 1 to numNodes do
        let bill = (TDMess(TopDownMessage(i.ToString(), "1", 1)))
        genArray.[0].Post(bill)

    for i = 1 to numNodes do
        let bill = (TDMess(TopDownMessage(i.ToString(), "111111", 2)))
        genArray.[0].Post(bill)*)

    for i = 1 to numNodes do
        let bill = (TDMess(TopDownMessage(i.ToString(), "111111111111111111111111111111", 3)))
        genArray.[0].Post(bill)

    //System.Threading.Thread.Sleep(5000)
    tcs.Task.Wait()

    //let mbg = new MailboxProcessor<TopDownMessage>(mailboxGeneral)
    (*let general = new MailboxProcessor<TopDownMessage>(fun inbox ->
        let rec loop count =
            async { let! msg = inbox.Receive()
                    printfn "Message is %A" msg.ID
                    return! loop(count + 1)}
        loop 0)*)



    0 // return an integer exit code
