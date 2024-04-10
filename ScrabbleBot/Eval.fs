module internal Eval

    open StateMonad
    open ScrabbleUtil

    (* Code for testing *)

    let hello = [('h', 4); ('e', 1); ('l', 1); ('l', 1); ('o', 1)] 
    let state = mkState [("x", 5); ("y", 42)] hello ["_pos_"; "_result_"]
    let emptyState = mkState [] [] []
    
    let add a b =
        a >>= fun x ->
        b >>= fun y ->
        ret (x + y)
    let div a b =
        a >>= fun x ->
        b >>= fun y ->
            match y with
            | 0 -> fail DivisionByZero
            | y -> ret (x / y)

    type aExp =
        | N of int
        | V of string
        | WL
        | PV of aExp
        | Add of aExp * aExp
        | Sub of aExp * aExp
        | Mul of aExp * aExp
        | Div of aExp * aExp
        | Mod of aExp * aExp
        | CharToInt of cExp

    and cExp =
       | C  of char  (* Character value *)
       | CV of aExp  (* Character lookup at word index *)
       | ToUpper of cExp
       | ToLower of cExp
       | IntToChar of aExp

    type bExp =             
       | TT                   (* true *)
       | FF                   (* false *)

       | AEq of aExp * aExp   (* numeric equality *)
       | ALt of aExp * aExp   (* numeric less than *)

       | Not of bExp          (* boolean not *)
       | Conj of bExp * bExp  (* boolean conjunction *)

       | IsVowel of cExp      (* check for vowel *)
       | IsLetter of cExp     (* check for letter *)
       | IsDigit of cExp      (* check for digit *)

    let (.+.) a b = Add (a, b)
    let (.-.) a b = Sub (a, b)
    let (.*.) a b = Mul (a, b)
    let (./.) a b = Div (a, b)
    let (.%.) a b = Mod (a, b)

    let (~~) b = Not b
    let (.&&.) b1 b2 = Conj (b1, b2)
    let (.||.) b1 b2 = ~~(~~b1 .&&. ~~b2)       (* boolean disjunction *)
    let (.->.) b1 b2 = (~~b1) .||. b2           (* boolean implication *) 
       
    let (.=.) a b = AEq (a, b)   
    let (.<.) a b = ALt (a, b)   
    let (.<>.) a b = ~~(a .=. b)
    let (.<=.) a b = a .<. b .||. ~~(a .<>. b)
    let (.>=.) a b = ~~(a .<. b)                (* numeric greater than or equal to *)
    let (.>.) a b = ~~(a .=. b) .&&. (a .>=. b) (* numeric greater than *)    

    let rec arithEval a : SM<int> =
        let binop f sm1 sm2 =
            sm1 >>= fun i1 ->
            sm2 >>= fun i2 ->
            ret (f i1 i2)
        let nonZeroSndArgBinop f sm1 sm2 =
            sm1 >>= fun i1 ->
            sm2 >>= fun i2 ->
            if i2 <> 0
            then ret (f i1 i2)
            else fail DivisionByZero
        match a with
        | N i -> ret i
        | V v -> lookup v
        | WL -> wordLength
        | PV a -> arithEval a >>= pointValue
        | Add (x, y) -> binop (+) (arithEval x) (arithEval y)
        | Sub (x, y) -> binop (-) (arithEval x) (arithEval y)
        | Mul (x, y) -> binop (*) (arithEval x) (arithEval y)
        | Div (x, y) -> nonZeroSndArgBinop (/) (arithEval x) (arithEval y)
        | Mod (x, y) -> nonZeroSndArgBinop (%) (arithEval x) (arithEval y)
        | CharToInt c -> charEval c >>= fun c -> ret (int c)
    and charEval c : SM<char> =
        match c with
        | C c -> ret c
        | CV a -> arithEval a >>= characterValue
        | ToUpper c -> charEval c >>= fun c -> ret (System.Char.ToUpper c)
        | ToLower c -> charEval c >>= fun c -> ret (System.Char.ToLower c)
        | IntToChar a -> arithEval a >>= fun i -> ret (char i)

    let rec boolEval b : SM<bool> =
        let binop f sm1 sm2 =
            sm1 >>= fun i1 ->
            sm2 >>= fun i2 ->
            ret (f i1 i2)
        match b with
        | TT -> ret true
        | FF -> ret false
        | AEq (x, y) -> binop (=) (arithEval x) (arithEval y)
        | ALt (x, y) -> binop (<) (arithEval x) (arithEval y)
        | Not b -> boolEval b >>= fun b -> ret (not b)
        | Conj (p, q) -> binop (&&) (boolEval p) (boolEval q)
        | IsVowel c -> charEval c >>= fun c -> ret ("aeiouAEIOU".Contains(c))
        | IsLetter c -> charEval c >>= fun c -> ret (System.Char.IsLetter c)
        | IsDigit c -> charEval c >>= fun c -> ret (System.Char.IsDigit c)


    type stmnt =                  (* statements *)
    | Declare of string           (* variable declaration *)
    | Ass of string * aExp        (* variable assignment *)
    | Skip                        (* nop *)
    | Seq of stmnt * stmnt        (* sequential composition *)
    | ITE of bExp * stmnt * stmnt (* if-then-else statement *)
    | While of bExp * stmnt       (* while statement *)

    let rec stmntEval stmnt : SM<unit> =
        match stmnt with
        | Declare str -> declare str
        | Ass (str, aExp) -> arithEval aExp >>= update str
        | Skip -> ret ()
        | Seq (stmnt1, stmnt2) -> stmntEval stmnt1 >>>= stmntEval stmnt2
        | ITE (bExp, stmnt1, stmnt2) ->
            boolEval bExp >>= fun b -> if b then stmntEval stmnt1 else stmntEval stmnt2
        | While (bExp, stmnt0) ->
            boolEval bExp >>= fun b ->
            if b then stmntEval stmnt0 >>>= stmntEval stmnt
            else ret ()

(* Part 3 (Optional) *)

    type StateBuilder() =

        member this.Bind(f, x)    = f >>= x
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Delay(f)      = f ()
        member this.Combine(a, b) = a >>= (fun _ -> b)
        
    let prog = new StateBuilder()

    let arithEval2 a = failwith "Not implemented"
    let charEval2 c = failwith "Not implemented"
    let rec boolEval2 b = failwith "Not implemented"

    let stmntEval2 stm = failwith "Not implemented"

(* Part 4 (Optional) *) 

    (*
    let stmntToSquareFun stm = failwith "Not implemented"

    let stmntToBoardFun stm m = failwith "Not implemented"

    type squareStmnt = Map<int, stmnt>
    let stmntsToSquare stms = failwith "Not implemented"

    type board = {
        center        : coord
        defaultSquare : square
        squares       : boardFun
    }

    let mkBoard c defaultSq boardStmnt ids = failwith "Not implemented"
    *)
    