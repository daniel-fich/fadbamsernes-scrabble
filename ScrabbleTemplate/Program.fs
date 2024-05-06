open BarelyFunctional

let time f =
    let start = System.DateTime.Now
    let res = f ()
    let finish = System.DateTime.Now
    (res, finish - start)

let readLines filePath = System.IO.File.ReadLines(filePath)

let spawnMultiples name dict bot =
    let rec aux =
        function 
        | 0 -> []
        | x -> (sprintf "%s%d" name x, dict, bot)::aux (x - 1)
   
    aux >> List.rev

[<EntryPoint>]
let main argv =
    ScrabbleUtil.DebugPrint.toggleDebugPrint false // Change to false to supress debug output

    System.Console.BackgroundColor <- System.ConsoleColor.DarkGray
    System.Console.ForegroundColor <- System.ConsoleColor.Black
    System.Console.Clear()

    let board      = ScrabbleUtil.InfiniteBoard.infiniteBoard ()

    let words     = readLines "../../../Dictionaries/English.txt"

    let handSize   = 7u
    let timeout    = None
    let tiles      = ScrabbleUtil.English.tiles 1u
    let seed       = None
    let port       = 13001

    let dictAPI =
        Some (Gaddag.empty, Gaddag.insert, Gaddag.step, Some Gaddag.reverse) 
        
    let (dictionary, time) =
        time (fun () -> ScrabbleUtil.Dictionary.mkDict words dictAPI) 

    // let players    = [("OxyphenButashit", dictionary,Scrabble.startGame); ("OxyphenButazone",dictionary,Oxyphenbutazone.Scrabble.startGame)]
    let players = spawnMultiples "BarelyFunctional" dictionary Scrabble.startGame 2


    do ScrabbleServer.Comm.startGame 
          board dictionary handSize timeout tiles seed port players
    
    ScrabbleUtil.DebugPrint.forcePrint ("Server has terminated. Press Enter to exit program.\n")
    System.Console.ReadLine () |> ignore

    0
