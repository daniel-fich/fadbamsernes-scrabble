module Gaddag
    open System
    type Dict = {
            letters : Map<char, Dict>
            isTerminal : bool
        }
        
    let empty =
        fun () -> {
            letters = Map.empty
            isTerminal = false
        }
    let ifNoneThenSome a =
        match a with
        | None -> empty()
        | Some(b) -> b

    let rec rotateFold (str: char list) (acc: char list list) =
        // let l = List.length str
        // match str with
        // | _ when str[l-1]='#' -> acc
        // | _ ->
        //     let x = str[idx]
        //     let r = [y; x] @ xs
        //     rotateFold r (r::acc)
        // | _ -> failwith "Will not be hit"
        
        // ^ fucking horrible
        // I got lazy don't hate
        let arr = List.toArray str
        let mutable acc1 = []
        for i in 1..List.length str-1 do
            for j in 0..i do
                let tmpi = arr[i]
                let tmpj = arr[j]
                arr[j] <- tmpi
                arr[i] <- tmpj
            acc1 <- Array.toList arr :: acc1
        acc1
        
    let insert (str: string) (state: Dict): Dict =
        let rec inner (str1: char list) cont (st: Dict) : Dict =
            match str1 with
            | [] -> cont {st with isTerminal = true }
            | x :: xs -> 
                let lower = Char.ToLower x
                let curr = Map.tryFind lower st.letters
                inner xs  (
                    fun next -> cont {
                        st with letters = Map.add lower next st.letters
                    }
                ) (ifNoneThenSome curr)
      
        let strs = rotateFold ('#'::(List.ofSeq str)) []
        
        let rec isrt a st =
            match a with
            | [] -> st
            | x :: xs -> isrt xs (inner x id st)
            
        isrt strs state

    let poundIfNot c cond =
        if not (cond c) then '#' else c
    
    let lookup (str: string) (state: Dict): bool =
        let rec inner (str1: char list) (st: Dict): bool =
            let curr = Char.ToLower >> fun x -> Map.tryFind x st.letters
            let exists = fun x -> Map.containsKey x st.letters
            match str1 with
            | [x] ->
                let letter = poundIfNot x exists
                (({letters = Map.empty; isTerminal = false}, curr letter) ||> Option.defaultValue).isTerminal
            | x :: xs ->
                let letter = poundIfNot x exists
                match curr letter with
                | None -> false
                | Some(e) ->
                    if letter = '#' then
                        inner (x::xs) e
                    else 
                        inner xs e
            | _ -> false // Should not be hit
        inner (List.ofSeq str) state
        
    let step c state =
        match state.letters |> Map.tryFind c with
        | None -> None
        | Some(e) -> Some(e.isTerminal, e)


    let reverse state = step '#' state
        