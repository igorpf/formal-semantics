module L1 = 
    
            
   
    type TmX = string
    type TmOp = 
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
        | TmOp of TmOp * Term * Term
        | TmIf of Term * Term * Term
        | TmX (*Variável X*)
        | TmApply of Term * Term (*e1 e2*)
        | TmFn of TmX * Term (*fn X:T -> e*)
        (*let*)
        (*let rec*)
        | TmNil
        | TmConcat of Term * Term (*e1::e2*)
        | TmIsZero of Term
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
        | TmIsZero ( TmInt 0 ) -> TmBool true
        | TmIsZero ( TmInt x ) when ( x > 0 ) -> TmBool false
        | TmIsZero ( t1 ) -> 
            let t1' = step t1 in
            TmIsZero ( t1' )
        | TmOp ( t, t1, t2) ->
            let t1' = step t1 in
            TmOp(t, t1', t2)
        (*como representar valores prontos??*)
        
        | _ -> raise NoRuleApplies

    let rec eval t =
        try let t' = step t
            in eval t'
            with NoRuleApplies -> t