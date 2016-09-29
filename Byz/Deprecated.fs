// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open System.IO

type TreeElement(elemVal, otherNode, rootNode:Node, ?someTEs) as self =
    //let elemVal = elemVal

    //let otherNodes = otherNodes

    //let rootNode = rootNode

    let mutable subTEs = 
        match someTEs with
        | Some (x:TreeElement[]) -> x
        | None -> [||]

    (*let rec stringifyTree (tree:TreeElement) = 
        let mutable retval = ""
        if subTEs.Length < 1 then
            if elemVal = "0" || elemVal = "1" then
                retval <- elemVal
            else 
                //retval <- rootNode.V0
                retval <- rootNode.V0
        else
            let returnString = String.concat "." [| for x in subTEs -> (stringifyTree x) |]
            retval <- returnString

        retval*)

    let rec stringifyTree (tree:TreeElement) = 
        let mutable retval = ""
        if subTEs.Length < 1 then
            if elemVal = "0" || elemVal = "1" then
                retval <- ":" + elemVal
            else 
                retval <- ":" + rootNode.V0
        else
            let returnString = rootNode.NodeID.ToString() + "." + (String.concat "." [| for x in subTEs -> (stringifyTree x) |])
            retval <- returnString

        retval

    member x.AttachTree tree = 
        subTEs <- tree

    member x.AttachTreeRet tree = 
        subTEs <- tree
        self

    member x.GetTreeString = 
        stringifyTree self

and Node(nodeID, initVal, isFaulty, ?someMessages, ?somev0) as self =
    let nodeID = Int32.Parse nodeID

    let messages =
        match someMessages with
        | Some x -> x
        | None -> [|"J"|]

    let v0 = 
        match somev0 with
        | Some x -> x
        | None -> "4"

    let initVal = initVal

    //let isFaulty = isFaulty

    member val Tree = TreeElement(v0, "bob", self) with get, set

    member x.NodeID
        with get () = nodeID

    (*member x.Tree
        with get () = Tree
        and set y = Tree <- y*)

    member x.V0
        with get () = v0

    member x.IsFaulty = 
        isFaulty = "1"

[<EntryPoint>]
let main argv = 
    //printfn "%A" argv
    let reader = File.OpenText(argv.[0])
    let mutable fileLine = reader.ReadLine()
    let mutable linePieces = fileLine.Split()
    let numNodes = Int32.Parse linePieces.[0]
    let v0 = linePieces.[1]
    let (nodeArray:Node[]) = Array.create numNodes (Node("0", "1", "2"))
    for i = 0 to numNodes - 1 do
        fileLine <- reader.ReadLine()
        linePieces <- fileLine.Split()
        if linePieces.[2] = "1" then
            nodeArray.[i] <- Node(linePieces.[0], linePieces.[1], linePieces.[2], linePieces.[3..], v0)
        else
            nodeArray.[i] <- Node(linePieces.[0], linePieces.[1], linePieces.[2])

    Array.sortInPlaceBy (fun (x:Node) -> x.NodeID) nodeArray

    let leaves = [|for x in 0..10 -> TreeElement(x.ToString(), "fred", nodeArray.[0]) |]

    let top = Array.reduce (fun x (y:TreeElement) -> y.AttachTreeRet [|x|]) leaves

    let mystring = top.GetTreeString
    
    printf "%A" mystring

    0 // return an integer exit code