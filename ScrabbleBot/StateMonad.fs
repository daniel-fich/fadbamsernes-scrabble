module internal StateMonad

    type Error = 
        | VarExists of string
        | VarNotFound of string
        | IndexOutOfBounds of int
        | DivisionByZero 
        | ReservedName of string           

    type Result<'a, 'b>  =
        | Success of 'a
        | Failure of 'b

    type State = { vars     : Map<string, int> list
                   word     : (char * int) list 
                   reserved : Set<string> }

    type SM<'a> = S of (State -> Result<'a * State, Error>)

    let mkState lst word reserved = 
           { vars = [Map.ofList lst];
             word = word;
             reserved = Set.ofList reserved }

    let evalSM (s : State) (S a : SM<'a>) : Result<'a, Error> =
        match a s with
        | Success (result, _) -> Success result
        | Failure error -> Failure error
    
    //let evalSMState (s : State) (S a : SM<'a>) = a s

    let bind (f : 'a -> SM<'b>) (S a : SM<'a>) : SM<'b> =
        S (fun s ->
              match a s with
              | Success (b, s') -> 
                match f b with 
                | S g -> g s'
              | Failure err     -> Failure err)


    let ret (v : 'a) : SM<'a> = S (fun s -> Success (v, s))
    let fail err     : SM<'a> = S (fun s -> Failure err)

    let (>>=)  x f = bind f x
    let (>>>=) x f = x >>= (fun () -> f)

    let push : SM<unit> = 
        S (fun s -> Success ((), {s with vars = Map.empty :: s.vars}))

    let pop : SM<unit> =
        S (fun s -> Success ((), {s with vars = List.tail s.vars}))

    let wordLength : SM<int> = S (fun s -> Success (List.length s.word, s))

    let characterValue (pos : int) : SM<char> =
        S (fun s ->
            if 0 <= pos && pos < List.length s.word
            then Success (s.word[pos] |> fst, s)
            else Failure (IndexOutOfBounds pos))

    let pointValue (pos : int) : SM<int> =
        S (fun s ->
            if 0 <= pos && pos < List.length s.word
            then Success (s.word[pos] |> snd, s)
            else Failure (IndexOutOfBounds pos))

    let lookup (x : string) : SM<int> = 
        let rec aux =
            function
            | []      -> None
            | m :: ms -> 
                match Map.tryFind x m with
                | Some v -> Some v
                | None   -> aux ms

        S (fun s -> 
              match aux (s.vars) with
              | Some v -> Success (v, s)
              | None   -> Failure (VarNotFound x))

    let declare (var : string) : SM<unit> =
        S (fun s ->
            if s.vars |> List.head |> Map.containsKey var
            then Failure (VarExists var)
            
            else if Set.contains var s.reserved
            then Failure (ReservedName var)
            
            else Success ((), {s with vars = (s.vars |> List.head |> Map.add var 0) :: List.tail s.vars}))
       
    let update (var : string) (value : int) : SM<unit> = // Not finished
        let rec updateVars = // Not tail recursive
            function
            | [] -> []
            | m :: ms ->
                if Map.containsKey var m
                then Map.add var value m :: ms
                else m :: updateVars ms
        
        S (fun s ->
            if List.fold (fun exists map -> exists || Map.containsKey var map) false s.vars // Checks if var exists
            then Success ((), {s with vars = updateVars s.vars})
            else Failure (VarNotFound var))
