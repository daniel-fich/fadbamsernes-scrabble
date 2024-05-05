module internal SolvingUtil
    open System
    open BasicUtils
    open ScrabbleUtil
    open Types
  
    let findWordWithOffset pos offset board dir =
        let rec aux pos dir (acc: char list) =
            if Map.containsKey pos board then
                let offset = computeOffset pos offset dir
                let c = Map.find pos board |> fst
                aux offset dir (c :: acc)
            else
                pos,acc
        aux pos dir []
    
    let findStartWordDir pos (board: Map<int*int,char*int>) direction =
        findWordWithOffset pos -1 board direction 
        
    let findEndWordDir pos (board: Map<int*int,char*int>) direction =
        let coords, res = findWordWithOffset pos 1 board direction
        coords, res |> List.rev
    
    let toCharListWithCoords word pos dir =
             word |> List.mapi (fun i c -> computeOffset pos i dir,c)
    let reBuildFromBoard charsWithPos state =
        ("", charsWithPos) ||> List.fold (fun acc (coords, c) ->
        let c = 
            if Map.containsKey coords state.board then
                Map.find coords state.board |> fst
            else
                c
        acc+(Char.ToString c))
         
    let generateApiMoveFromCoordCharList lst =
        ("", lst) ||> List.fold (fun acc (coord, c) ->
            acc+generateValidMoveForApiFromLetter c coord)
    
    let rec removeOverlappingLettersOnBoardAndValidate word pos (state: state) dir =
        let charsWithPos = toCharListWithCoords word pos dir
        let boardString = reBuildFromBoard charsWithPos state 
        let crossDir = otherDir dir
        let crossDirectionValid = (true, charsWithPos) ||> List.fold (fun acc (coords, c) ->
                let strt,start = findStartWordDir coords state.board crossDir
                let _,endWrd = findEndWordDir coords state.board crossDir
                let endWrd = endWrd[1..]
                if endWrd |> (@) start |> List.length > 1 then
                    // let charsWithPos = toCharListWithCoords (start @ endWrd) strt crossDir
                    // let boardString = reBuildFromBoard charsWithPos state
                    acc && Dictionary.lookup (String.Concat (start@endWrd)) state.dict
                else
                    acc) 
        let crossDirectionValid = crossDirectionValid 
        if Dictionary.lookup boardString state.dict && crossDirectionValid then
            Some(charsWithPos |> List.filter (fun (coord,_) -> not (Map.containsKey coord state.board)))
        else
            None
            
           

