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
    let ifNoneThenSome a = Option.defaultValue (empty()) a
        
    let rec rotateFold (str: 'a list) (acc: 'a list list) =
        let rec swapElements lst i j =
            let tmp = lst
            let ret = lst |> List.mapi (fun idx elm ->
                if idx = i then tmp[j]
                else if idx = j then tmp[i]
                else elm)
            ret
                
        let rec aux str i acc =
            let rec inner j str =
                if j = i then
                    str
                else
                    let swapped = (swapElements str i j)
                    inner (j+1) swapped
            if i = (List.length str) - 1 then
                acc
            else
                let swapped = (inner 0 str)
                aux swapped (i+1) (swapped::acc)
        aux str 1 []
        
    let rec rotateFold2 (str: 'a list) =
        let mutable acc1 = []
        for i in 0..List.length str do
            let res = (str, []) ||> List.fold (fun v a -> v[i%List.length str] :: a)
            acc1 <- res::acc1
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
      
        let str = List.ofSeq str
        let strs = rotateFold ('#'::str) []
        
        let rec isrt a st =
            match a with
            | [] -> st
            | x :: xs -> isrt xs (inner x id st)
            
        isrt ((List.rev str)::strs) state

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
        match state.letters |> Map.tryFind (Char.ToLower c) with
        | None -> None
        | Some(e) -> Some(e.isTerminal, e)

    let allowed c state =
        match state.letters |> Map.tryFind c with
        | None -> false
        | Some(_) -> true

    let reverse state = step '#' state
        