open System
module L1 = 
    
            
     
    type Ident = string
    type Operator = 
        | OpPlus
        | OpMinus
        | OpMult
        | OpDiv
        | OpLT (*<*)
        | OpLE (*<=*)
        | OpEQ (*==*)
        | OpNEQ (*!=*)
        | OpGT (*>*)
        | OpGE (*>=*)
    type Term = 
        | TmBool of bool         
        | TmInt of int
        | TmOp of Operator * Term * Term
        | TmIf of Term * Term * Term
        | TmX of Ident (*Variável X*)
        | TmApply of Term * Term (*e1 e2*)
        | TmFn of Ident * Term (*fn X:T -> e*)        
        | TmLet of Ident * Term * Term (*let x = e1 in e2*)
        | TmLetRec of Ident * Term * Term (*let rec f = (fn y => e1) in e2  *)
        | TmNil 
        | TmCons of Term * Term (*e1::e2*)
        | TmIsEmpty of Term
        | TmHd of Term 
        | TmTl of Term
        | TmRaise
        | TmTry of Term * Term 

    exception NoRuleApplies
    

    let rec step t = 
        match t with
        | TmIf ( TmBool true , t2 , t3 ) -> t2
        | TmIf ( TmBool false, t2 , t3 ) -> t3
        | TmIf ( t1 , t2 , t3 ) -> 
            let t1' = step t1 in
                TmIf ( t1' , t2 , t3 )
        (*Op rules*)
        | TmOp (OpPlus, TmInt x, TmInt y) -> TmInt (x+y)
        | TmOp (OpMinus, TmInt x, TmInt y) -> TmInt (x-y)
        | TmOp (OpMult, TmInt x, TmInt y) -> TmInt (x*y)
        | TmOp (OpDiv, TmInt x, TmInt 0) -> TmRaise
        | TmOp (OpDiv, TmInt x, TmInt y) when (y <> 0) -> TmInt (x/y)
        | TmOp (OpLT, TmInt x, TmInt y) -> TmBool (x<y)
        | TmOp (OpLE, TmInt x, TmInt y) -> TmBool (x<=y)
        | TmOp (OpEQ, TmInt x, TmInt y) -> TmBool (x=y)
        | TmOp (OpNEQ, TmInt x, TmInt y) -> TmBool (x<>y)
        | TmOp (OpGT, TmInt x, TmInt y) -> TmBool (x>y)
        | TmOp (OpGE, TmInt x, TmInt y) -> TmBool (x>=y)
        | TmOp ( t, TmInt x, t2) ->
            let t2' = step t2 in
            TmOp(t, TmInt x, t2')
        | TmOp ( t, t1, t2) ->
            let t1' = step t1 in
            TmOp(t, t1', t2)
        
        | TmHd (TmCons (t1, t2)) -> t1
        | TmTl (TmCons (t1, t2)) -> t2
        | TmIsEmpty (TmNil) -> TmBool true
        | TmIsEmpty (TmCons(t,t1)) -> TmBool false

        | TmTry (TmRaise, e2) -> e2
        (*como representar valores prontos??*)
        
        | _ -> raise NoRuleApplies

    let rec eval t =
        try let t' = step t
            in eval t'
            with NoRuleApplies -> t
    let c = eval ((TmHd(TmCons (TmInt 5, TmNil))))
    Console.WriteLine("{0}", c)