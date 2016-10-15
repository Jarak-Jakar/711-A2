// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open FSharp.Control

let tcs = new System.Threading.Tasks.TaskCompletionSource<bool>()

let bar = new System.Threading.Barrier(1)

let factorial n = 
    let rec loop i acc = 
        match i with
        | 0 | 1 -> acc
        | _ -> loop (i - 1) (acc * i)
    loop n 1

type TopDownMessage(generalID, message : string, roundNumber) = 

    let messageSent = false

    member x.ID : String = 
        generalID

    member x.Message = 
        message

    member x.RoundNumber = 
        roundNumber

type BottomUpMessage(nextRound : int) = 
    
    member x.RoundNumber = 
        nextRound

type StepMessage(nextRound, genArray : General[]) = 
    
    member x.RoundNumber = 
        nextRound

    member x.GenArray =
        genArray

and  Message =
    | TDMess of TopDownMessage
    | SMess of StepMessage
    | BUMess of BottomUpMessage

and  General(Id, v0, initVal, isFaulty, numGenerals, numRounds, ?someMessages) = 
    let messages = 
        match someMessages with
        | Some (x : String[]) -> Array.init numRounds (fun y -> String.concat " " x.[(y * numGenerals)..(y * numGenerals + numGenerals - 1)])
        | None -> Array.empty

    let messagesArray = Array.init numRounds (fun x -> (Array.create (factorial numGenerals / factorial (numGenerals - x - 1)) (initVal)))

    // Used when placing parts of incoming messages into the array
    let rec compindex roundNumber numGenerals sender messageSize i genless  = 
        if roundNumber = 0 then
            sender
        else 
            let a = ((i * genless) / messageSize)
            if a < sender then
                let b = a * messageSize
                b + (compindex (roundNumber - 1) (numGenerals - 1) (sender - 1) (messageSize / numGenerals) (i - (a * (numGenerals - 1))) (genless - 1))
            else
                let b = (a + 1) * messageSize
                b + (compindex (roundNumber - 1) (numGenerals - 1) sender (messageSize / numGenerals) (i - (a * (numGenerals - 1))) (genless - 1))

    // Used to determine where each part of an outgoing message should be selected from
    let rec compindex2 roundNumber numGenerals sender messageSize i genless  = 
        if roundNumber = 1 then
            if i < sender then
                i
            else
                i + 1
        else 
            let a = (i / (numGenerals - roundNumber))
            if a < sender then
                let b = a * messageSize
                b + (compindex2 (roundNumber - 1) numGenerals (sender - 1) (messageSize / numGenerals) (i - (a * (genless - 1))) (genless - 1))
            else
                let b = (a + 1) * messageSize
                b + (compindex2 (roundNumber - 1) numGenerals sender (messageSize / numGenerals) (i - (a * (genless - 1))) (genless - 1))

    let mb = 
        MailboxProcessor.Start (fun inbox ->
            let rec loop count = 
                async { let! (msg : Message) = inbox.Receive()
                        match msg with
                        | TDMess mesg -> 
                            let sender = (Int32.Parse mesg.ID) - 1
                            let rn = mesg.RoundNumber - 1
                            let msgSize = (factorial numGenerals) / (numGenerals * (factorial (numGenerals - rn - 1)))
                            let mutable index = 0
                            for i = 0 to mesg.Message.Length - 1 do
                                index <- compindex rn (numGenerals - 1) sender msgSize i (numGenerals - 1)
                                let x = mesg.Message.[i]
                                if x = '0' || x = '1' then
                                    messagesArray.[rn].[index] <- mesg.Message.[i].ToString()
                                else
                                    messagesArray.[rn].[index] <- v0

                            //printfn "genArray = %A" messagesArray
                            //printfn "hi bill"
                            if count >= (numGenerals - 1) then
                                //tcs.SetResult(true)
                                //bar.SignalAndWait()
                                bar.SignalAndWait()
                                //return! loop 0
                                //genArray.[rn,i].[sender] <- Int32.Parse mesg.Message[i]
                        | SMess mesg ->
                            if isFaulty = "0" then
                                let mutable sendMessage = initVal

                                // Work out sendMessage here
                                if mesg.RoundNumber > 1 then
                                    let messageSize = (factorial numGenerals) / (numGenerals * (factorial (numGenerals - mesg.RoundNumber)))
                                    sendMessage <- String.concat "" (Array.init messageSize (fun i -> messagesArray.[mesg.RoundNumber - 2].[compindex2 (mesg.RoundNumber - 1) numGenerals ((Int32.Parse Id) - 1) (messageSize / (numGenerals - mesg.RoundNumber + 1)) i (numGenerals - 1)]))

                                Array.iter (fun (x : General) -> x.Post(TDMess(TopDownMessage(Id, sendMessage, mesg.RoundNumber)))) mesg.GenArray
                                
                                Console.WriteLine("{0} {1} > {2}", mesg.RoundNumber, Id, (String.concat " " (Array.create numGenerals sendMessage)))

                            else
                                let sendMessage = messages.[mesg.RoundNumber - 1].Split ' '
                                Array.iteri (fun i (x : General) -> x.Post(TDMess(TopDownMessage(Id, sendMessage.[i], mesg.RoundNumber)))) mesg.GenArray
                                
                                Console.WriteLine("{0} {1} > {2}", mesg.RoundNumber, Id, messages.[mesg.RoundNumber - 1])

                            return! loop 0
                        | BUMess mesg -> 
                            
                            let takeVote startInd endInd roundNumber = 
                                let counts = Array.countBy id messagesArray.[roundNumber].[startInd..endInd] |> Array.sortBy (fun x -> fst x)
                                if counts.Length = 1 then
                                    fst counts.[0]
                                else
                                    if (snd counts.[0]) > (snd counts.[1]) then
                                        "0"
                                    elif (snd counts.[0]) < (snd counts.[0]) then
                                        "1"
                                    else
                                        v0

                            let mutable rn = mesg.RoundNumber - 1
                            for i = (numRounds - 1) downto 1 do
                                let chunkSize = numGenerals - rn + 1
                                for j = 0 to messagesArray.[rn - 2].Length - 1 do
                                    messagesArray.[rn - 2].[j] <- takeVote (j * chunkSize) (j * chunkSize + chunkSize - 1) (rn - 1)
                                rn <- rn - 1
                            
                            let newVal = takeVote 0 (messagesArray.[0].Length - 1) 0

                            let outString = String.concat " " ((Array.map (fun x -> String.concat "" x) messagesArray) |> Array.rev)
                            
                            Console.WriteLine("{0} {1} : {2} {3}", mesg.RoundNumber, Id, outString, newVal)
                            bar.SignalAndWait()
                        //| _ -> printfn "Matched something other than a proper message"
                        return! loop (count + 1)}
            loop 0)

    member x.Post(msg) = mb.Post(msg)

    member x.IsFaulty =
        isFaulty = "1"

    member x.ID = 
        Id

[<EntryPoint>]
let main argv = 
    let reader = File.OpenText(argv.[0])
    let mutable fileLine = reader.ReadLine()
    let mutable linePieces = fileLine.Split()
    let numNodes = Int32.Parse linePieces.[0]
    let v0 = linePieces.[1]
    let genArray = Array.create numNodes (General("8", v0, "9", "1", 4, 2))
    let numRounds = int (Math.Floor((float (numNodes - 1)) / 3.0)) + 1
    for i = 0 to numNodes - 1 do
        fileLine <- reader.ReadLine()
        linePieces <- fileLine.Split()
        if linePieces.[2] = "1" then
            genArray.[i] <- General(linePieces.[0], v0, linePieces.[1], linePieces.[2], numNodes, numRounds, linePieces.[3..])
        else
            genArray.[i] <- General(linePieces.[0], v0, linePieces.[1], linePieces.[2], numNodes, numRounds)

        bar.AddParticipant() |> ignore

    Array.sortInPlaceBy (fun (x : General) -> x.ID) genArray

    for i = 1 to numRounds do
        Array.iter (fun (x : General) -> x.Post(SMess(StepMessage(i, genArray)))) genArray
        bar.SignalAndWait()

    Array.iter (fun (x: General) -> x.Post(BUMess(BottomUpMessage(numRounds + 1)))) genArray
    bar.SignalAndWait()

    0 // return an integer exit code
