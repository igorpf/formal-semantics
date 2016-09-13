open System
module L1 = 
    
            
   
    type Ident = string
    type TmOperator = 
        | TmPlus
        | TmMinus
        | TmMult
        | TmDiv
        | TmLT (*<*)
        | TmLE (*<=*)
        | TmEQ (*==*)
        | TmNEQ (*!=*)
        | TmGT (*>*)
        | TmGE (*>=*)
    type Term = 
        | TmBool of bool         
        | TmInt of int
        | TmOp of TmOperator * Term * Term
        | TmIf of Term * Term * Term
        | TmX of Ident (*Variável X*)
        | TmApply of Term * Term (*e1 e2*)
        | TmFn of Ident * Term (*fn X:T -> e*)
        (*let*)
        (*let rec*)
        | TmNil 
        | TmConcat of Term * Term (*e1::e2*)
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
        | TmOp (TmPlus, TmInt x, TmInt y) -> TmInt (x+y)
        | TmOp (TmMinus, TmInt x, TmInt y) -> TmInt (x-y)
        | TmOp (TmMult, TmInt x, TmInt y) -> TmInt (x*y)
        | TmOp (TmDiv, TmInt x, TmInt 0) -> TmRaise
        | TmOp (TmDiv, TmInt x, TmInt y) when (y <> 0) -> TmInt (x/y)
        | TmOp (TmLT, TmInt x, TmInt y) -> TmBool (x<y)
        | TmOp (TmLE, TmInt x, TmInt y) -> TmBool (x<=y)
        | TmOp (TmEQ, TmInt x, TmInt y) -> TmBool (x=y)
        | TmOp (TmNEQ, TmInt x, TmInt y) -> TmBool (x<>y)
        | TmOp (TmGT, TmInt x, TmInt y) -> TmBool (x>y)
        | TmOp (TmGE, TmInt x, TmInt y) -> TmBool (x>=y)
        | TmOp ( t, TmInt x, t2) ->
            let t2' = step t2 in
            TmOp(t, TmInt x, t2')
        | TmOp ( t, t1, t2) ->
            let t1' = step t1 in
            TmOp(t, t1', t2)
        
        | TmHd (TmConcat (t1, t2)) -> t1
        | TmTl (TmConcat (t1, t2)) -> t2
        | TmIsEmpty (TmNil) -> TmBool true
        | TmIsEmpty (TmConcat(t,t1)) -> TmBool false

        | TmTry (TmRaise, e2) -> e2
        (*como representar valores prontos??*)
        
        | _ -> raise NoRuleApplies

    let rec eval t =
        try let t' = step t
            in eval t'
            with NoRuleApplies -> t
    let c = eval ((TmHd(TmConcat (TmInt 5, TmNil))))
    Console.WriteLine("{0}", c)