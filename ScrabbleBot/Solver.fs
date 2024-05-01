module internal Solver

    open System
    open MultiSet
    open ScrabbleUtil.Dictionary
    open State

    type Direction =
        | horizontal = 0
        | vertical = 1
    
    type Rack = char list   
    
    
    let uintToLetter (n: uint32) =
        let baseAscii = int 'A'
        let offset = int n - 1
        if offset >= 0 && offset < 26 then
            char (baseAscii + offset)
        else
            ' '
    let asciiLetterToAlphabetPos (letter: char): uint32 =
        let uppercaseLetter = Char.ToUpper letter
        let asciiA = uint32 'A'
        let asciiLetter = uint32 uppercaseLetter
        if asciiLetter >= asciiA && asciiLetter <= asciiA + 25u then
            asciiLetter - asciiA + 1u
        else
            failwith "Input is not an uppercase letter"

    let conditionalAppend item acc stmt =
        if stmt then
            item::acc
        else acc
    let computeOffset (x, y) pos direction =
        if direction = Direction.horizontal then
            (x+pos,y)
        else
            (x, y+pos)
    let unionMap (map1: Map<int*int,char>) (map2:Map<int*int,char>) =
        (map2, map1) ||> Map.fold (fun acc key value -> Map.add key value acc)
    let lOnOldArc letter state =
        Option.isSome (step letter state.dict)
    let recordPlay anchor pos dir word acc =
        (computeOffset anchor pos dir, word) :: acc
    let rec gen1 anchor pos word state acc dir =
        let offset = computeOffset anchor pos dir
        if Map.containsKey offset state.board then
            let letter,_ = Map.find offset state.board
            goon1 anchor pos letter word (step letter state.dict) state acc dir
        else if not (isEmpty state.hand) then
            (acc, getKeys state.hand) ||> List.fold (fun acc' item ->
                let letter = uintToLetter item
                let nextDict = (step letter state.dict)
                if nextDict |> Option.isSome then
                    let nextHand = removeSingle item state.hand
                    goon1 anchor pos letter word (step letter state.dict) {state with hand = nextHand} acc' dir
                else
                    acc'
            )
        else
            acc
    and goon1 anchor pos letter word newArc state acc dir =
        let leftTaken = Map.containsKey (computeOffset anchor (pos-1) dir) state.board
        let rightTaken = Map.containsKey (computeOffset anchor (pos+1) dir) state.board
        let rightFromAnchor = Map.containsKey (computeOffset anchor (1) dir) state.board
        if pos <= 0 then
            let word = letter :: word
            let acc = 
                if lOnOldArc letter state && not leftTaken && not rightFromAnchor then
                    recordPlay anchor pos dir word acc
                else
                    acc
            if Option.isSome newArc then
                let newArc = Option.get newArc |> snd
                let acc =
                    if not leftTaken then
                        gen1 anchor (pos-1) word {state with dict = newArc} acc dir
                    else
                        acc
                let newArc = reverse newArc
                if Option.isSome newArc && not leftTaken && not rightTaken then
                    let newArc = Option.get newArc |> snd
                    gen1 anchor 1 word {state with dict = newArc} acc dir
                else
                    acc
            else
                acc
        else
            let word = word @ [letter]
            let acc =
                if lOnOldArc letter state && not rightTaken then
                    recordPlay anchor (pos - word.Length + 1) dir word acc
                else
                    acc
            if Option.isSome newArc && not rightTaken then
                let newArc = Option.get newArc |> snd
                gen1 anchor (pos+1) word {state with dict = newArc} acc dir
            else
                acc
            
    let rec gen (x,y) (pos: int) (word: char list) (state:state) (acc) direction =
        let offset = computeOffset (x,y) pos direction
        if Map.containsKey offset state.board then
            let letter,_ = Map.find offset state.board
            // let isTerminal, nextDict = step letter state.dict |> Option.get
            goon pos (x,y) letter word (step letter state.dict) acc state direction
        else if not (isEmpty state.hand) then
            ([], getKeys state.hand) ||> List.fold (fun acc' item ->
                let letter = uintToLetter item
                let nextDict = (step letter state.dict)
                if nextDict |> Option.isSome then
                    let nextHand = removeSingle (asciiLetterToAlphabetPos letter) state.hand
                    // let isTerminal, nextDict = step letter state.dict |> Option.get
                    (goon pos (x,y) letter word nextDict acc {state with hand = nextHand } direction)
                else
                    acc'
            )
        else
            // printfn "ooooooga:  %A" word
            // printfn "ooooooga:  %A" acc
            acc
    and goon pos (x,y) letter word newArc acc state direction =
        let x',y' = computeOffset (x,y) pos direction
        let leftTaken = Map.containsKey (computeOffset (x,y) (pos-1) direction) state.board
        let rightTaken = Map.containsKey (computeOffset (x,y) (pos+1) direction) state.board
        let rightFromAnchor = Map.containsKey (computeOffset (x,y) (1) direction) state.board
        if pos <= 0 then
            let newWord = letter :: word
            // let isEndOfWord = if Option.isSome newArc then newArc |> Option.get |> fst else false
            let newAcc =
                if (Option.isSome (step letter state.dict) && not leftTaken) then
                    (newWord |> Array.ofList |> String.Concat) :: acc
                else
                    acc
            // let newAcc =
            //     if Option.isSome (step letter state.dict) && not leftTaken && not rightFromAnchor then
            //         Map.add (x', y') letter acc
            //     else
            //         acc
            if Option.isSome newArc then
                let newDict = Option.get newArc |> snd
                let fstStmnt =
                    if not leftTaken then
                        gen (x, y) (pos-1) newWord {state with dict = newDict} newAcc direction 
                    else
                        newAcc
                let newNewDict = reverse newDict
                let sndStmnt =
                    if Option.isSome newNewDict && not leftTaken && not rightTaken then
                        let newNewNewDict = Option.get newNewDict |> snd
                        (gen (x,y) 1 newWord {state with dict = newNewNewDict} newAcc direction)
                    else
                        newAcc
                // unionMap fstStmnt sndStmnt
                fstStmnt @ sndStmnt
            else
                newAcc
        else
            let newWord = word @ [letter]
            // let newAcc = conditionalAppend ((x',y'),newWord |> Array.ofList |> String.Concat) acc (lookup (Char.ToString letter) state.dict && not rightTaken)
            let newAcc =
                if (Option.isSome (step letter state.dict) && not leftTaken) then
                    (newWord |> Array.ofList |> String.Concat) :: acc
                else
                    acc
            // let newAcc =
            //     if Option.isSome (step letter state.dict) && not rightTaken then
            //         Map.add (x', y') letter acc
            //     else
            //         acc
            if Option.isSome newArc && not rightTaken then
                let newDict = Option.get newArc |> snd
                gen (x, y) (pos+1) newWord {state with dict = newDict } newAcc direction
            else newAcc
            
    let find_anchors ((x,y): int*int) (board: Map<int*int, char*int>) =
           let viable = [x,y+1;x,y-1;x+1,y;x-1,y]
           ([],viable) ||>
               List.fold (fun acc elm ->
                   if Map.containsKey elm board then
                       acc
                   else
                       elm :: acc
               )
            
    let generate_moves (anchor: int*int) (dir: Direction) (state: state) =
        // let ret = gen (x,y) 0 [] state Map.empty dir
        let ret = gen1 anchor 0 [] state [] dir
        (((0,0),[]), ret) ||> List.fold (fun acc (coords, word) ->
            if List.length word > List.length (snd acc) then
                coords, word
            else acc)
            
    let fbm (dir: Direction) (state: state) =
        let b = state.board
        let k = b.Keys
        let res = ([], Seq.toList k) ||> List.fold (fun acc pos ->
            let a = find_anchors pos b
            let moves =
                ([], a) ||> List.fold (fun acc pos ->
                    let res = generate_moves pos dir state
                    if List.length (snd res) > 0 then
                        res :: acc
                    else
                        acc)
            acc @ moves
            )
        
        res
            
        // let ret = generate_moves (-1,0) Direction.horizontal state
        // ret
            